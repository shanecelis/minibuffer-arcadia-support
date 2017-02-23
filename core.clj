;; Copyright (c) 2017 Shane Celis, @shanecelis[1]
;;
;; Licensed under the MIT License
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;; [1]: https://twitter.com/shanecelis

(ns minibuffer.lisp.core
    (:require clojure.string
              clojure.repl)
    (:use arcadia.core
          arcadia.repl
          [clojure.string :refer [trim]])
  (:import
   [clojure.lang Symbol]
   [UnityEngine Time Mathf Debug]
   [RSG Promise IPromise]
   [seawisphunter.minibuffer Minibuffer Command Prompt Keymap ICompleter]))

;; Thanks, Joseph (@selfsame), for the repl environment tip!
(def repl-env (atom (arcadia.repl/env-map)))

(defn repl-eval-print-string
  "This repl is used by the eval-expression command.

  (def result-string (repl-eval-print-string \"(+ 1 2)\"))"
  [s]
  (let [{:keys [result env]} (arcadia.repl/repl-eval-print @repl-env s)]
       (reset! repl-env env)
       result))

(defn do-with-minibuffer
  "Run (f Minibuffer/instance) when Minibuffer/instance is available.

   FIXME: I don't think defering works currently. I'm not sure any state can be
  saved when hitting stop then play in Unity."
  [f]
  (if (nil? Minibuffer/instance)
      (do
          (log "defering do-with-minibuffer")
          ;; This is no longer good.
          (Minibuffer/OnStart (sys-action [Minibuffer] [m] (f m))))
    (f Minibuffer/instance)))

(defmacro with-minibuffer
  "Minibuffer/instance is only available when a scene with Minibuffer is running.
This form will defer execution until such an instance is available.

  (with-minibuffer minibuffer
    (.Message minibuffer \"Minibuffer instance available!\"))"
    [minibuffer-var & body]
    `(do-with-minibuffer (fn [~minibuffer-var] ~@body)))

(defmacro generate-generic-type
  "Like generate-generic-delegate but just provides the type.

e.g. (generate-generic-type 'Dictionary [String String])
       -> |Dictionary`2[System.String,System.String]|"
  [typename typesyms]
  (let [types (map (fn [tsym]
                       (if-let [t (clojure.lang.CljCompiler.Ast.HostExpr/MaybeType tsym false)]
                               t
                               (throw (ArgumentException. (format "Unable to convert '%s' to a type." tsym)))))
                   typesyms)
       ftype (symbol (str typename "`" (count types) "[" (clojure.string/join "," types) "]"))]
       ftype))


;; http://stackoverflow.com/questions/20751565/executing-code-in-another-clojure-namespace-why-is-eval-required
;; (defmacro with-ns2 [ns & body]
;;           `(let [orig-ns# (ns-name *ns*)]
;;                 (try
;;                  (in-ns ~ns)
;;                  ~@body
;;                  (finally (in-ns orig-ns#)))))

;; http://stackoverflow.com/questions/20751565/executing-code-in-another-clojure-namespace-why-is-eval-required
(defmacro with-ns
    "Evaluates body in another namespace. ns is either a namespace
   object or a symbol. This makes it possible to define functions in
   namespaces other than the current one."
  [ns & body]
  `(binding [*ns* (the-ns ~ns)]
            ~@body))

(defmacro with-repl-ns
    [& body]
  `(with-ns ~(:*ns* @repl-env)
            ~@body))

(defmacro with-def-ns
    [& body]
    `(with-ns ~*ns*
              ~@body))

;; I'd really prefer if this weren't a macro.  Seems possible.
(defmacro make-delegate
  "Creates a delegate that may be a Func<...>, Action<...>, or Action based on
  typeargs and args. A Func<...> is created if there is one more typearg than
  arg. An Action is created if there are no typeargs or args."
    [typeargs args f]
    ;; I'm going to run the delegates in the 'user namespace.
    ;; This should probably be configurable.  This got complicated.
    ;; It used to just be `(~f ~@args)'.
    ;;
    ;; Should I instead try to run them in the repl-env? YES.
    (let [f-call `;(with-def-ns
                   ;(log "delegate ns now " *ns*)
                   (try (~f ~@args)
                        (catch Exception e#
                               (Debug/LogException e#)))
                   ;)
                   ]
    (cond
      (some nil? typeargs)   (throw
                              (ArgumentException.
                               (str "There are nils in typeargs: "
                                    (pr-str typeargs))))
      (some #{'&} args)      (throw
                              (ArgumentException.
                               "Cannot have variable length arguments '&' in command."))
      (= (count typeargs)
         (+ 1 (count args))) `(sys-func ~typeargs ~args ~f-call)
         (empty? args)       `(gen-delegate Action [] ~f-call)
         (= (count typeargs)
            (count args))    `(sys-action ~typeargs ~args ~f-call)
            :else
            (throw (ArgumentException.
                    (str "Typeargs and args must be equal length or typeargs may have "
                         "one more element at the end to represent its return type."))))))

(defmacro separate-types
  "Given a function signature with type annotations do the following:

  (separate-types ^Int64 do-it [^String a ^Object b c])
    -> {:typeargs [String Object nil] :typeresult Int64}"
  [f args]
          `{:typeargs (->> '~args
                           (mapv meta)
                           (mapv :tag))
            :typeresult (:tag (meta '~f))})

(defmacro separate-types-oldform
  "Given a function signature with type annotations do the following:

  (separate-types ^Int64 do-it [^String a ^Object b c])
    -> [String Object nil Int64]

  This is suitable for input into the macro make-delegate."
  [f args]
          `(let [h# (separate-types ~f ~args)]
                (if (:typeresult h#)
                    (conj (:typeargs h#) (:typeresult h#))
                  (:typeargs h#))))

(defn- gen-setter
  ([map key]
        (gen-setter map key (name key)))
  ([map key property-name inst]
        `(if-let [v# (~key ~map nil)]
                 (~(symbol (str ".set_" property-name)) ~inst v#))))

(defn- kebabs-to-camels
  "Convert from kebab-case to camelCase."
  [s]
  (if (nil? s)
      nil
    (clojure.string/replace s #"-([a-z])" #(clojure.string/upper-case (%1 1)))))

(defmacro make-map-constructor
  "Construct an object of type using a map that sets its various properties.

Create constructor:
  (make-map-constructor make-command Command [:name :description])

Use constructor:
  (make-command {:name \"example-cmd\" :description \"Exemplary command\" })
"
  [make-name type keys]
  (let [inst (gensym 'instance)]
       `(defn ~make-name [~'attrs]
          (let [~inst (new ~type)]
               ~@(map (fn [k] (gen-setter
                               'attrs
                               k
                               (kebabs-to-camels (name k))
                               inst)) keys)
               ~inst))))

(defn- select-prompt-attrs [map]
  (select-keys
   map
   [:prompt :input :history :completer :require-match :require-coerce :completions
    :ignore :default-value]))

(make-map-constructor
 ^:private make-prompt-
 Prompt
 [:prompt :input :history :completer :require-match :require-coerce :completions
  :ignore :default-value])

(defn make-prompt
 "Create a Prompt object. Accepts the following attributes:

 [:prompt :input :history :completer :require-match :require-coerce :completions :ignore]"
  [attrs]
  (let [p-attrs (select-prompt-attrs attrs)]
       (if (empty? p-attrs)
           nil                               ; No prompt if nil.
         (make-prompt- (assoc p-attrs
                              :completions
                              (if-let [comps (:completions p-attrs nil)]
                                      (into-array String comps)))))))


(make-map-constructor
 ^:private make-command
 Command
 [:name :description :brief-description :group-name :hidden :keymap :key-binding
  :signature :parameter-names :prompts :defined-in])

(defn register-command
  "Register a Minibuffer command."
  [cmd-name-or-attrs typeargs args f]
  (let [attrs* (if (string? cmd-name-or-attrs)
                   {:name cmd-name-or-attrs}
                   cmd-name-or-attrs)
       attrs (assoc
              attrs*
              :parameter-names
              (if-let [pnames (:parameter-names attrs* nil)]
                      (into-array String
                                  (into ["closure"]
                                        pnames)))
              :prompts
              (if-let [ps (:prompts attrs* nil)]
                      (into-array Prompt
                                  ;; add a nil for the closure param
                                  (conj (map make-prompt ps)
                                        nil)))
              )]
                                        ;; (log "register prompt type" (type (:prompts attrs)))
   (with-minibuffer
    m
    (.RegisterCommand m
                      (make-command attrs)
                      (eval `(make-delegate ~typeargs ~args ~f))))))

(defmacro defcmd
  "Define a Minibuffer command and function.

  (defcmd name doc-string? [params*] body)

  Type annotations should be used on parameters. Minibuffer makes extensive use
 of the type information. Consider the following example.

  e.g. (defcmd ^Int64
          say-hello
          \"Says hello. Return int.\"
          [^String name]
          (message \"Hello, %s!\" name)
          1)

  The function `say-hello` will be defined as if it were a normal `defn` but it
  may additionally be run interactively by typing `M-x say-hello` which will
  then prompt for a string. Functions with a String output will be `message`'d
  automatically. Functions with a IEnumerator output will be run as coroutines."
  [fn-name & defnargs]
  (let [[docstring args & body] (if (string? (first defnargs))
                                    defnargs
                                  (conj (seq defnargs) nil))]
       `(do
        (defn ~fn-name
          ~@defnargs)
            (register-command {
                              :name ~(name fn-name)
                              :description ~docstring
                              :key-binding ~(:key-binding (meta fn-name) nil)
                              :defined-in
                              ~(format "namespace %s" (ns-name *ns*))
                              :signature ~(str "(defn " (name fn-name) " ["
                                            (clojure.string/join " "
                                              (map name args)) "] ...)")
                              :parameter-names ~(mapv name args)
                              :prompts ~(let [prompts
                                           (->> args
                                                (mapv meta)
                                                (mapv select-prompt-attrs))]
                                                   (if (every? empty? prompts)
                                                       nil
                                                     prompts))}
                          (separate-types-oldform ~fn-name ~args)
                          '~args
                          '~fn-name)
      #'~fn-name)))

(defn echo
  "Print string to the echo area."
  [str]
  (with-minibuffer m
                   (.Echo m str)))

(defn message
  "Print a message to the echo area and log to *Messages* buffer."
  ([msg]
   (with-minibuffer m
                    (.Message m msg))
   msg)
  ([fmt & strings]
        (let [msg (apply format fmt strings)]
             (message msg))))

(defn message-or-buffer
  "Depending on the size of the output, message or use a buffer."
  [buffer-name msg]
  (with-minibuffer m
                   (.MessageOrBuffer m buffer-name msg)))



(defn get-keymap
  "Return a keymap of the given name or null if it doesn't exist. May opt to
  create one if it doesn't exist."
  ([name createIfNecessary]
         (if Minibuffer/instance
             (.GetKeymap Minibuffer/instance name createIfNecessary)))
  ([name]
   (get-keymap name false)))


(defn get-buffer
  "Return a buffer of the given name or null if it doesn't exist. May opt to
  create one if it doesn't exist."
  ([name createIfNecessary]
         (if Minibuffer/instance
             (.GetBuffer Minibuffer/instance name createIfNecessary)))
  ([name]
   (get-buffer name false)))

(defn get-key-binding
  "Return the command bound to key-sequence if any."
  [keymap key-sequence]
  (.get_Item keymap key-sequence))

(defn set-key-binding
  "Bind the key-sequence to the command."
  [keymap key-sequence command-name]
  (.set_Item keymap key-sequence command-name))

(defn define-key
  "Bind the key-sequence to the command in a particular keymap."
  [keymap-or-name key-sequence command-name]
  (let [keymap (if (string? keymap-or-name)
                   (get-keymap keymap-or-name)
                 keymap-or-name)]
                 (if Minibuffer/instance
                     (set-key-binding keymap key-sequence command-name))))

;; http://stackoverflow.com/questions/9273333/in-clojure-how-to-apply-a-macro-to-a-list
;; It always comes to this.
(defmacro functionize
  "Turn a macro into a function via eval."
  ([macro]
   `(fn [& args#] (eval (cons '~macro args#))))
  ([name macro]
   `(defn ~name [& args#] (eval (cons '~macro args#)))))

(defn- var->ns [v]
  (:ns (meta v)))

(defn- fqns
  "Fully qualified namespace for symbol or nil.
e.g. (fqns 'defcmd) -> #object[Namespace 0x9bf96000 \"minibuffer.lisp.core\"]"
  [s]
  (if-let [var (resolve s)]
          (->> var meta :ns)))

(defn fully-qualified-symbol
  "(fully-qualified-symbol \"defcmd\") -> 'minibuffer.lisp.core/defcmd"
  [string]
  (if-let [ns (fqns (symbol string))]
          (symbol (name (ns-name ns)) string)))

(defcmd ^{:key-binding "C-h f"} describe-function
  "Describe Clojure function."
  [^{:prompt "Describe function: " :history "function"
     :completer "function" :require-match true}
   ^Symbol function]
   ;(log "type df " (type function))
   (if-let [var (ns-resolve (:*ns* @repl-env) function)]
           #_(eval-expression (format "(doc %s)" (name function))
                            "*doc*")
           (message-or-buffer "*doc*"
                              (trim (with-out-str
                                     (with-repl-ns
                                      (eval `(clojure.repl/doc ~function))))))
           (message "No such function \"%s\"." function)))

(defcmd ^{:key-binding "C-h s"} show-source
  "Show source code if available."
  [^{:prompt "Show source: " :history "function" :completer "source"
     :require-match true}
   ^Symbol function]
   ;(log "type show source" (type function))
   (if-let [var (ns-resolve (:*ns* @repl-env) function)]
           #_(eval-expression (format "(source %s)" (name function))
                            "*source*")
           (message-or-buffer "*source*"
                              (trim
                               (with-out-str
                                (with-repl-ns
                                 (eval `(clojure.repl/source ~function))))
                               #_(with-repl-ns
                                (with-out-str
                                 (clojure.repl/source-fn (fully-qualified-symbol (name function))))))
                              )
           (message "No such function \"%s\"." function)))

(defn make-func-completer
  "Convert a fn into a ICompleter. The completer accepts the current input and
  returns a list of potential matches (does not have to be sorted).

The coercer accepts two arguments, the selected string and the desired type.

  e.g. (f \"Sh\") -> (\"Shane\" \"Shawn\" \"Shannon\")
       (f \"Shane\" clojure.lang.Symbol) -> 'Shane"
  [completer-fn]
  (CompleterEntity. (reify ICompleter
                           (Complete [this input]
                                     (into-array String (completer-fn input)))
                           (Coerce [this item desired-type]
                                   (completer-fn item desired-type)))))

(defn make-list-completer
  "Convert a list of strings into a list-completer."
  [lst]
  (CompleterEntity. (ListCompleter. (into-array String lst))))

(defmacro into-dict
    "Convert a map into a particular type of dictionary."
    [type map]
  `(let [d# (new ~type)]
       (doseq [[k# v#] ~map]
              (.set_Item d# (name k#) v#)
              ;;(.Add d# (name k#) v#)
              ;; Add will throw an error if it tries to overwrite an existing key.
              )
       d#))

(defn into-dict-string-string
  "Convert map into a Dictionary<String,String> object."
  [map]
  (into-dict |Dictionary`2[String,String]| map))

(defn make-dict-completer
  "Convert a map into a DictCompleter."
     [map]
     (CompleterEntity. (DictCompleter. (into-dict-string-string map))))

;; Not generic
;; (defn then
;;   [cs-promise f]
;;   (let [ptype (generate-generic-type IPromise [String])
;;        atype (generate-generic-type Action [String])
;;        meth (.GetMethod ptype "Then" (into-array Type [atype]))]
;;        (.Invoke meth cs-promise (into-array Object [(sys-action [String] [s] (f s))]))))

;; Generic
(defn then
  "Given a RSG.IPromise<T> run (f result-value) when the promise resolves."
  [ipromise f]
  (let [ptype (type ipromise)
       of-type (.GetGenericArguments ptype)
       atype (eval `(generate-generic-type Action ~of-type))
       meth (.GetMethod ptype "Then" (into-array Type [atype]))
       arg #_(eval `(sys-action ~(apply vector of-type) [~'s] (~f ~'s)))
       (. clojure.lang.GenDelegate Create atype f)
       ]
       (.Invoke meth ipromise (into-array Object [arg]))))

(defn pcatch
  "Given an RSG.IPromise p, if p is rejected, run (f e) where e is the
  exception.

Note: named pcatch to not conflict with try/catch builtin."
  [ipromise f]
  (.Catch ipromise (sys-action [Exception] [e] (f e))))

(defn read-from-minibuffer
  "Read a string from the minibuffer prompt. This returns an IPromise.

e.g. (then (read-from-minibuffer \"Who are you? \") #(message \"Hi, %s.\" %))"
  [prompt & {:keys [input history completer require-match require-coerce]
             :or {input "" history nil completer nil
                  require-match false require-coerce false}}]
  (with-minibuffer
   m
   (.Read m prompt input history completer require-match require-coerce)
   ;; I was considering using Clojure's promises, but they seem
   ;; anemic: not then-able, no fail state. Will stick with
   ;; IPromise library.

   ;; (let [p (promise)]
   ;;      (then
   ;;       (.Read m prompt input history completer require-match require-coerce)
   ;;       (fn [result]
   ;;           (deliver p result)))
   ;;      p)
   ))

(defn read-type
  [type prompt & {:keys [input history completer require-match require-coerce]
                  :or {input "" history nil completer nil
                       require-match false require-coerce false}}]
  (let [read-method
    (MinibufferExtensions/GetMethodGeneric
     Minibuffer "Read"
     (into-array Type [type])
     (into-array Type [String String String String Boolean Boolean]))]
     (with-minibuffer
      m
      (.Invoke read-method m (into-array Object [prompt input history completer
                                                 require-match require-match])))))

(defn set-completer
  "Add a completer to Minibuffer. Any fn, list, or map will be automatically
coerced to a completer type unless `:coerce? false' is added to the arguments."
  [name completer & {:keys [coerce?]
                     :or {coerce? true}}]
  (let [c (if coerce?
              (cond
               (fn? completer)  (make-func-completer completer)
               (seq? completer) (make-list-completer completer)
               (map? completer) (make-dict-completer completer)
               :default         completer)
            completer)]
            ;(.Add (.completers Minibuffer/instance) name c)
            (.set_Item (.completers Minibuffer/instance) name c)
            c))

(def fn-completer-namespaces
     (atom ['arcadia.core 'minibuffer.lisp.core 'minibuffer.lisp.example 'user]))

;; (defn filter-ns-map [ns excluded]
;;   (let [excluded* (set (map (comp ns-name the-ns) excluded))]
;;       (->> (the-ns ns)
;;         (ns-refers)
;;         (vals)
;;         (filter (fn [kv]))
;;         (first)
;;         (var->ns)
;;         (ns-name)
;;         (contains? excluded*)
;;         ))
;;   )

(defn filter-functions [amap]
  (into {} (filter #(let [v (val %)]
                         (and (bound? v) (fn? @v))) amap)))

(defn filter-variables [amap]
  (into {} (filter #(let [v (val %)]
                         (and (bound? v) (not (fn? @v)))) amap)))

(defn filter-sources [amap]
  (into {} (filter #(let [k (key %)]
                         (try (with-repl-ns
                               (not= "Source not found\n"
                                     (count (clojure.repl/source-fn k))))
                              (catch Exception e
                                     false)))
                   amap)))

;; (ns-refers (the-ns ns))
(defn filter-ns
  "Filter out namespaces from something like an `(ns-publics ns)' map"
  [excluded amap]
  (let [excluded* (set (map (comp ns-name the-ns) excluded))
        m amap]
       (select-keys m
                    (for [[k v] m :when
                         (not (contains? excluded* (ns-name (var->ns v))))]
                         k))))

(def symbol-completer-exclude-namespaces (atom ['clojure.core 'clojure.repl]))

(defn make-symbol-completer
  "Create a symbol completer. Accepts a filter function that will take a
dictionary of symbols and vars (like you get from ns-publics)."
  ([]
   (make-symbol-completer identity))
  ([filter-map]
   (fn ([input]
        (filter #(clojure.string/starts-with? % input)
                (map (comp name first)
                     (filter-map
                      #_(mapcat #(ns-publics (the-ns %))
                              @fn-completer-namespaces)
                      (filter-ns @symbol-completer-exclude-namespaces
                                 (merge (ns-refers (:*ns* @repl-env))
                                        (ns-interns (:*ns* @repl-env))))))))
       ([item desired-type]
              (cond (= clojure.lang.Symbol desired-type)
                    (symbol item)
                    (= String desired-type)
                    item
                    :else
                    (throw (MinibufferException.
                            (format "Unable to convert \"%s\" to desired type %s." item desired-type))))))))

(defn repl-setup
  "Called by LispCommands.cs after minibuffer.core.lisp is loaded. Sets up
  namespaces and completers."
  []
  (repl-eval-print-string "(in-ns 'user)")
  (repl-eval-print-string "(use 'minibuffer.lisp.core 'minibuffer.lisp.example 'arcadia.core 'clojure.repl)")
  (repl-eval-print-string "(import [UnityEngine Time Mathf Debug]")
  ;; This is a static completer.  It will know the symbols that
  ;; we started with.
  #_(set-completer "function"
                 (map name (mapcat clojure.repl/dir-fn @fn-completer-namespaces)))
  ;; This is a live completer.
  (set-completer "Symbol" (make-symbol-completer))
  (set-completer "function" (make-symbol-completer filter-functions))
  (set-completer "variable" (make-symbol-completer filter-variables))
  (set-completer "source" (make-symbol-completer filter-sources))
  ;(in-ns 'user)
  )

;; This is equivalent to the C# coded "eval-expression" command
;; in LispCommands.cs.
;;
;; (defcmd ^{:key-binding "M-:"} eval-expression

;;   "Evaluate a Clojure expression and show its result."
;;   [ ^{:prompt "Eval: " :history "expression" }
;;   ^String expression
;;   ^{:default-value "*eval*"}
;;   ^String buffer-name]
;;   (message-or-buffer buffer-name (trim (repl-eval-print-string expression))))
