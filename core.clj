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
    (:use arcadia.core
          arcadia.repl
          ) ;; use or require?  I don't know the difference.
  (:import
   [UnityEngine Time Mathf Debug]
   [seawisphunter.minibuffer Minibuffer Command Prompt Keymap ICompleter]))

;(log "minibuffer.lisp.core reloaded")

(defn do-with-minibuffer
  "Run (f Minibuffer/instance) when Minibuffer/instance is available.

   FIXME: I don't think defering works currently. I'm not sure any state can be saved when hitting stop then play in Unity."
  [f]
  (if (nil? Minibuffer/instance)
      (do
        (log "defering do-with-minibuffer")
        (Minibuffer/OnStart (gen-delegate Action [] (f Minibuffer/instance))) )
    (f Minibuffer/instance)))

(defmacro with-minibuffer
  "Minibuffer/instance is only available when a scene with Minibuffer is running. This form will defer execution until such an instance is available.

  (with-minibuffer minibuffer
    (.Message minibuffer \"Minibuffer instance available!\"))"
    [minibuffer-var & body]
    `(do-with-minibuffer (fn [~minibuffer-var] ~@body)))

;; I'd really prefer if this weren't a macro.  Seems possible.
(defmacro make-delegate
  "Creates a delegate that may be a Func<...>, Action<...>, or Action based on
  typeargs and args. A Func<...> is created if there is one more typearg than
  arg. An Action is created if there are no typeargs or args."
    [typeargs args f]
    (cond
     (some nil? typeargs)                    (throw
                                              (ArgumentException.
                                               (str "There are nils in typeargs: "
                                                    (pr-str typeargs))))
     (some #{'&} args)                       (throw
                                              (ArgumentException. "Cannot have variable length arguments '&' in command."))
     (= (count typeargs) (+ 1 (count args))) `(sys-func ~typeargs ~args (~f ~@args))
     (empty? args)                           `(gen-delegate Action [] (~f))
     (= (count typeargs) (count args))       `(sys-action ~typeargs ~args (~f ~@args))
     :else (throw (ArgumentException.
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

(defn- select-prompt-attrs [map]
  (select-keys
   map
   [:prompt :input :history :completer :require-match :require-coerce :completions :ignore]))

(make-map-constructor
 ^:private make-prompt-
 Prompt
 [:prompt :input :history :completer :require-match :require-coerce :completions :ignore])

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
 [:name :description :brief-description :group-name :hidden :keymap :key-binding :signature :parameter-names :prompts :defined-in])

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

(defn message
  "Print a message to the echo area."
  ([msg]
   (with-minibuffer m
                    (.Message m msg))
   msg)
  ([fmt & strings]
        (let [msg (apply format fmt strings)]
             (message msg)
             )))

;; Thanks, Joseph (@selfsame), for the repl environment tip!
(def repl-env (atom (arcadia.repl/env-map)))

(defn repl-eval-print-string
  "This repl is used by the eval-expression command.

  (def result-string (repl-eval-print-string \"(+ 1 2)\"))"
  [s]
  (let [{:keys [result env]} (arcadia.repl/repl-eval-print repl-env s)]
       (reset! repl-env env)
       result))





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
(defmacro functionize
    ([macro]
     `(fn [& args#] (eval (cons '~macro args#))))
  ([name macro]
         `(defn ~name [& args#] (eval (cons '~macro args#)))))
(functionize doc-fn clojure.repl/doc)

(defcmd ^{:key-binding "C-h f"} describe-function
  [^String ^{:prompt "Describe function: " :history "function" :completer "function" :require-match true} function]
  (message (with-out-str (eval `(clojure.repl/doc ~(symbol function))))))

(defcmd ^{:key-binding "C-h s"} describe-source
  [^String ^{:prompt "Describe source: " :history "function" :completer "function" :require-match true} function]
  (message (with-out-str (eval `(clojure.repl/source ~(symbol function))))))


(defn make-func-completer [completer-fn]
  (CompleterEntity. (reify ICompleter
                           (Complete [this pattern]
                                     (completer-fn pattern))
                           (Coerce [this item desired-type]
                                   item))))

(defn make-list-completer [lst]
  (CompleterEntity. (ListCompleter. (into-array String lst))))

(defn add-completer [name completer]
  (.Add (.completers Minibuffer/instance) name completer))


(def fn-completer-namespaces ['arcadia.core 'minibuffer.lisp.core])

(defn repl-setup
  "Called after minibuffer.core.lisp is loaded. Allows for a little setup like
  using namespaces."
  []
  (repl-eval-print-string "(use 'minibuffer.lisp.core 'arcadia.core)")
  (add-completer "function" (make-list-completer (map name (mapcat clojure.repl/dir-fn fn-completer-namespaces)))))

;; This is equivalent to the C# coded "eval-expression" command
;; in LispCommands.cs.
;;
;; (defcmd eval-expression
;;   "Evaluate an expression and show its result in the echo-area."
;;   [ ^{:prompt "Eval: " :history "expression" :key-binding "M-;"}
;;     ^String expression]
;;   (message (trim (repl-eval-print-string expression))))
