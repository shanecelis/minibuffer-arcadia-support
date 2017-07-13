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
    (:use minibuffer.lisp.internal
          arcadia.core
          arcadia.repl
          [clojure.string :refer [trim]])
  (:import
   [clojure.lang Symbol]
   [UnityEngine Time Mathf Debug]
   [RSG Promise IPromise]
   [SeawispHunter.MinibufferConsole Minibuffer Command Prompt Keymap ICompleter Variable CompleterEntity MinibufferException]
   [SeawispHunter.MinibufferConsole.Extensions MinibufferExtensions]))

(defmacro with-minibuffer
  "Minibuffer/instance is only available when a scene with Minibuffer is running.
This form will defer execution until such an instance is available.

  (with-minibuffer minibuffer
    (.Message minibuffer \"Minibuffer instance available!\"))"
    [minibuffer-var & body]
    `(do-with-minibuffer (fn [~minibuffer-var] ~@body)))

(defmacro register-command
  "Register a Minibuffer command."
  [cmd-name-or-attrs typeargs args f]
  `(register-command-fn ~cmd-name-or-attrs (make-delegate ~typeargs ~args ~f)))

(defmacro register-variable
  "Register a Minibuffer variable."
  [type variable-name-or-attrs f-get f-set]
  `(register-variable-fn ~type ~variable-name-or-attrs
                         (sys-func [~type] [] (~f-get))
                         (sys-action [~type] [~'value] (~f-set ~'value))))

(defmacro defvar
    "Define a minibuffer variable. This variable is mutable therefore the value
is wrapped in `(atom value)'."
    ([name value]
           `(defvar ~name nil ~value))
    ([name doc value]
           `(do (def ~name ~@(if doc [doc] []) (atom ~value))
                (register-variable ~(type value) {:name ~(clojure.core/name name) :description ~doc
                                                  :defined-in ~(format "namespace %s" (ns-name *ns*)) }
                                   (fn [] (deref ~name))
                                   (fn [~'v] (reset! ~name ~'v))))))

(defmacro defparam
    "Define a minibuffer parameter. The parameter is not mutable and will
provide an error message if `M-x edit-variable param-name' is attempted."
  ([name value]
         `(defparam ~name nil ~value))
  ([name doc value]
         `(do (def ~name ~@(if doc [doc] []) ~value)
              (register-variable
               ~(type value) {:name ~(clojure.core/name name) :description ~doc
                              :defined-in ~(format "namespace %s" (ns-name *ns*)) }
               (fn [] ~name)
               (fn [~'v]
                   (message "Cannot change parameter value. Consider using defvar."))))))

;; BUG messed up defcmd by changing register-command into a macro
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
                                  (conj (seq defnargs) nil))
       typeargs (eval `(separate-types-oldform ~fn-name ~args))]
       `(do
            (defn ~fn-name
              ~@defnargs)
            ;;(log "register-command-fn" ~(name fn-name))
            (register-command-fn
             {
             :name ~(name fn-name)
             :description ~docstring
             :key-binding ~(:key-binding (meta fn-name) nil)
             :defined-in
             ~(format "namespace %s" (ns-name *ns*))
             :signature ~(str "(defn " (name fn-name) " ["
                              (clojure.string/join
                               " "
                               (map name args)) "] ...)")
             :parameter-names ~(mapv name args)
             :prompts ~(let [prompts
                         (->> args
                              (mapv meta)
                              (mapv select-prompt-attrs))]
                              (if (every? empty? prompts)
                                  nil
                                prompts))
             }
             (make-delegate ~typeargs
                            ~args
                            ~fn-name))
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

(defn to-buffer
  "Output to a buffer."
  [buffer-name content]
  (with-minibuffer m
    (.ToBuffer m buffer-name content)))

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

;; Not generic
;; (defn then
;;   [cs-promise f]
;;   (let [ptype (generate-generic-type IPromise [String])
;;        atype (generate-generic-type Action [String])
;;        meth (.GetMethod ptype "Then" (into-array Type [atype]))]
;;        (.Invoke meth cs-promise (into-array Object [(sys-action [String] [s] (f s))]))))

;; Generic
(defn then
  "Given a RSG.IPromise<T> run (f result-value) when the promise resolves. A new
  promise is returned."
  [ipromise f]
  (let [ptype   (type ipromise)
        of-type (.GetGenericArguments ptype)
        atype   (eval `(generate-generic-type Action ~of-type))
        meth    (.GetMethod ptype "Then" (into-array Type [atype]))
        arg     (. clojure.lang.GenDelegate Create atype f)]
    (.Invoke meth ipromise (into-array Object [arg]))))

(defn pcatch
  "Given an RSG.IPromise p, if p is rejected, run (f e) where e is the
  exception.

Note: named pcatch to not conflict with try/catch builtin."
  [ipromise f]
  (.Catch ipromise (sys-action [Exception] [e] (f e))))

(defn read-from-minibuffer
  "Read a string from the minibuffer prompt. This returns an IPromise.

e.g. (then (read-from-minibuffer \"Who are you? \")
           #(message \"Hi, %s.\" %))"
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

(defn read-type-from-minibuffer
  "Read from the minibuffer but of a particular type. Basically Minibuffer.Read<T>().

(then (read-type-from-minibuffer Int64 \"How old are you?\")
      #(message \"%d is a fine age.\" %))
"
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

(def symbol-completer-exclude-namespaces
     "These namespaces are excluded from the symbol completer. This can be altered."
     (atom ['clojure.core 'clojure.repl]))

(defn make-symbol-completer
  "Create a symbol completer. Accepts a filter function that will take a
dictionary of symbols and vars (like you get from ns-publics) and return a
dictionary of symbols and vars."
  ([]
   (make-symbol-completer identity))
  ([filter-map]
   (fn ([input]
        (filter #(clojure.string/starts-with? % input)
                (map (comp name first)
                     (filter-map
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
  "Called by LispCommands.cs after minibuffer.lisp.core is loaded. Sets up
  namespaces and completers."
  []
  (repl-eval-print-string "(in-ns 'user)")
  (repl-eval-print-string "(use 'minibuffer.lisp.core 'minibuffer.lisp.example 'arcadia.core 'clojure.repl)")
  (repl-eval-print-string "(import [UnityEngine Time Mathf Debug]")

  ;; This is a static completer.  It will know the symbols that
  ;; we started with.
  ;;(set-completer "function"
  ;;               (map name (mapcat clojure.repl/dir-fn @fn-completer-namespaces)))

  ;; This is a live completer.
  (set-completer "Symbol" (make-symbol-completer))
  (set-completer "function" (make-symbol-completer filter-functions))
  (set-completer "variable" (make-symbol-completer filter-variables))
  (set-completer "source" (make-symbol-completer filter-sources)))

(defcmd ^{:key-binding "C-h f"} describe-function
  "Describe Clojure function."
  [^{:prompt "Describe function: " :history "function"
     :completer "function" :require-match true}
   ^Symbol function]
   ;(log "type df " (type function))
   (if-let [var (ns-resolve (:*ns* @repl-env) function)]
           (message-or-buffer "*doc*"
                              (trim (with-out-str
                                     (with-repl-ns
                                      (eval `(clojure.repl/doc ~function))))))
           (message "No such function \"%s\"." function)))

(defcmd ^{:key-binding "C-."} show-source
  "Show source code if available."
  [^{:prompt "Show source: " :history "function" :completer "source"
     :require-match true}
   ^Symbol function]
   ;(log "type show source" (type function))
   (if-let [var (ns-resolve (:*ns* @repl-env) function)]
           (message-or-buffer "*source*"
                              (trim
                               (with-out-str
                                (with-repl-ns
                                 (eval `(clojure.repl/source ~function)))))
                               #_(with-repl-ns
                                (with-out-str
                                 (clojure.repl/source-fn (fully-qualified-symbol (name function)))))
                              )
           (message "No such function \"%s\"." function)))

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
