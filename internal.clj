(ns minibuffer.lisp.internal
  (:require clojure.string
            clojure.repl)
  (:use arcadia.core
        arcadia.repl
        [clojure.string :refer [trim]])
  (:import
   [clojure.lang Symbol]
   [UnityEngine Time Mathf Debug]
   [RSG Promise IPromise]
   [seawisphunter.minibuffer Minibuffer Command Prompt Keymap ICompleter Variable]))

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

;; I'd really prefer if this weren't a macro.  Seems possible.
(defmacro make-delegate
  "Creates a delegate that may be a Func<...>, Action<...>, or Action based on
  typeargs and args. A Func<...> is created if there is one more typearg than
  arg. An Action is created if there are no typeargs or args."
    [typeargs args f]
    ;; I was going to run the delegates in the 'user namespace.
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
      (empty? args)          `(gen-delegate Action [] ~f-call)
      (= (count typeargs)
         (count args))       `(sys-action ~typeargs ~args ~f-call)
         :else
         (throw (ArgumentException.
                 (str "Typeargs and args must be equal length or typeargs may have "
                      "one more element at the end to represent its return type."))))))



;; I'd really prefer if this weren't a macro.  Seems possible.
(defmacro get-delegate-type
  "Creates a delegate that may be a Func<...>, Action<...>, or Action based on
  typeargs and args. A Func<...> is created if there is one more typearg than
  arg. An Action is created if there are no typeargs or args."
    [typeargs args]
    ;; I was going to run the delegates in the 'user namespace.
    ;; This should probably be configurable.  This got complicated.
    ;; It used to just be `(~f ~@args)'.
    ;;
    ;; Should I instead try to run them in the repl-env? YES.
    (cond
     (some nil? typeargs)   (throw
                             (ArgumentException.
                              (str "There are nils in typeargs: "
                                   (pr-str typeargs))))
     (some #{'&} args)      (throw
                             (ArgumentException.
                              "Cannot have variable length arguments '&' in command."))
     (= (count typeargs)
        (+ 1 (count args))) `(generate-generic-type "System.Func" ~typeargs)
     (empty? args)          |System.Action|
     (= (count typeargs)
        (count args))       `(generate-generic-type "System.Action" ~typeargs)
        :else
        (throw (ArgumentException.
                (str "Typeargs and args must be equal length or typeargs may have "
                     "one more element at the end to represent its return type.")))))



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

(defn select-prompt-attrs [map]
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
 make-command
 Command
 [:name :description :brief-description :group-name :hidden :keymap :key-binding
  :signature :parameter-names :prompts :defined-in])

(make-map-constructor
 make-variable
 Variable
 [:name :description :brief-description :dynamic :defined-in])

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

(defn filter-functions [amap]
  (into {} (filter #(let [v (val %)]
                         (and (bound? v) (fn? @v))) amap)))

(defn filter-variables [amap]
  (into {} (filter #(let [v (val %)]
                         (and (bound? v) (not (fn? @v)))) amap)))

(defn filter-sources [amap]
  (into {} (filter #(let [k (key %)]
                         (try (with-repl-ns
                               (not= "Source not found"
                                     #_(clojure.repl/source-fn k)
                                     (trim
                                      (with-out-str
                                       (with-repl-ns
                                        (eval `(clojure.repl/source ~k)))))))
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

(defn register-variable-fn [type variable-name-or-attrs get-func set-action]
  (let [attrs (if (string? variable-name-or-attrs)
                  {:name variable-name-or-attrs}
                  variable-name-or-attrs)
       generic-meth (.GetMethod Minibuffer "RegisterVariable")
       meth (.MakeGenericMethod generic-meth (into-array Type [type])) ]
       (do-with-minibuffer
        (fn [m]
            (.Invoke meth m
                     (into-array Object
                                 [(make-variable attrs)
                                 get-func
                                 set-action]))))))

(defn register-command-fn
  "Register a Minibuffer command."
  [cmd-name-or-attrs delegate-f]
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
                                        nil))))]
   (do-with-minibuffer
    (fn [m]
        (.RegisterCommand m
                          (make-command attrs)
                          delegate-f)))))
