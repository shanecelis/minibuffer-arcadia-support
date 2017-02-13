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
          arcadia.repl) ;; use or require?  I don't know the difference.

  (:import
   [UnityEngine Time Mathf Debug]
   [seawisphunter.minibuffer Minibuffer Command]))

;(log "minibuffer.lisp.core reloaded")

;; gen-delegate, generate-generic-delegate, sys-func, and sys-action taken from
;; core-clr.clj and reworked from macros to be functions.

(defn- my-gen-delegate
  [type f]
	(clojure.lang.GenDelegate/Create type f))


;; (clojure.string/join "," coll) will substitute for str-join, I think.
(defn- str-join    ;; clojure.string not yet loaded
  [coll]
  (loop [sb (StringBuilder. (str (first coll)))
        more (next coll)]
        (if more
            (recur (-> sb (.Append ",") (.Append (str (first more))))
                   (next more))
                   (str sb))))

(defn- my-generate-generic-delegate
  [typename typesyms f]
  (let [types (map (fn [tsym] (clojure.lang.CljCompiler.Ast.HostExpr/MaybeType tsym false)) typesyms)
        ftype (symbol (str typename "`" (count types) "[" (str-join types) "]"))]
       (my-gen-delegate ftype f)))

(defn- my-sys-func
  "Translates to a gen-delegate for a System.Func<,...> call"
  [typesyms f]
  (my-generate-generic-delegate "System.Func" typesyms f))

(defn- my-sys-action
  "Translates to a gen-delegate for a System.Action<,...> call"
  [typesyms f]
  (my-generate-generic-delegate "System.Action" typesyms f))

(defn do-with-minibuffer
  "Run (f Minibuffer/instance) when Minibuffer/instance is available.

   FIXME: I don't think defering works currently. I'm not sure any state can be saved when hitting stop then play in Unity."
  [f]
  (if (nil? Minibuffer/instance)
      (do
        ;(log "defering do-with-minibuffer")
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
     (some #{'&} args)                       (throw
                                              (ArgumentException. "Cannot have variable length arguments '&' in command."))
     (= (count typeargs) (+ 1 (count args))) `(sys-func ~typeargs ~args (~f ~@args))
     (empty? args)                           `(gen-delegate Action [] (~f))
     (= (count typeargs) (count args))       `(sys-action ~typeargs ~args (~f ~@args))
     :else (throw (ArgumentException.
                   (str "Typeargs and args must be equal length or typeargs may have "
                        "one more element at the end to represent its return type.")))))

(defn- my-make-delegate
    "Creates a delegate that may be a Func<...>, Action<...>, or Action based on
  typeargs and args. A Func<...> is created if there is one more typearg than
  arg. An Action is created if there are no typeargs or args."
  [typeargs args f]
  (cond
   (some #{'&} args)                       (throw
                                            (ArgumentException. "Cannot have variable length arguments '&' in command."))
   (= (count typeargs) (+ 1 (count args))) (my-sys-func typeargs f))
   (empty? args)                           (my-gen-delegate Action f)
   (= (count typeargs) (count args))       (my-sys-action typeargs f))
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

(defn register-command
  "Register a Minibuffer command."
  [cmd-name-or-attrs typeargs args f]
  (let [attrs (if (string? cmd-name-or-attrs)
                  {:name cmd-name-or-attrs}
                  cmd-name-or-attrs)]
   (with-minibuffer
    m
    (.RegisterCommand m
                      (doto (Command. (:name attrs nil))
                            (.set_description (:description attrs nil))
                            (.set_keySequence (:keysequence attrs nil))
                            (.set_signature   (:signature attrs nil))
                            (.set_parameterNames
                             (if (:parameter-names attrs nil)
                                 ;; There's an extra "closure "parameter in the
                                 ;; generated method.
                                 (into-array String (into ["closure"]
                                                          (:parameter-names attrs nil)))
                               nil)))
                      (eval `(make-delegate ~typeargs ~args ~f))))))
(defn my-register-command
  "Register a Minibuffer command."
  [cmd-name-or-attrs typeargs args f]
  (let [attrs (if (string? cmd-name-or-attrs)
                  {:name cmd-name-or-attrs}
                  cmd-name-or-attrs)]
   (with-minibuffer
    m
    (.RegisterCommand m
                      (doto (Command. (:name attrs nil))
                            (.set_description (:description attrs nil))
                            (.set_keySequence (:keysequence attrs nil))
                            (.set_signature   (:signature attrs nil))
                            (.set_parameterNames
                             (if (:parameter-names attrs nil)
                                 ;; There's an extra "closure "parameter in the
                                 ;; generated method.
                                 (into-array String (into ["closure"]
                                                          (:parameter-names attrs nil)))
                               nil)))
                      (eval `(make-delegate ~typeargs ~args ~f))))))

(defmacro defcmd
    "Define a Minibuffer command and function. The docstring is required.
  The typeargs define what C# types the function accepts and optionally what it
  returns. Minibuffer makes extensive use of the type information. Consider the
  following example.

  e.g. (defcmd ^Int64 say-hello \"Says hello. Return int.\" 
          [^String x]
          (message \"Hello, \" x)
          1)

  The function `say-hello` be defined as if it were a normal `defn` but it may
  additionally be run interactively by typing `M-x say-hello` which will then
  prompt for a string. Functions with a String output will be `message`'d
  automatically. Functions with a IEnumerator output will be run as coroutines."
  [fn-name docstring args & body]
  `(do
       (defn ~fn-name
         ~docstring
         ~args
         ~@body)
       (register-command { :name ~(name fn-name)
                         :description ~docstring
                         :signature ~(str "(defn " (name fn-name) " ["
                                         (clojure.string/join " " (map name args)) "] ...)")
                         :parameter-names ~(mapv name args) }
                         (separate-types-oldform ~fn-name ~args)
                         '~args
                         '~fn-name)
     #'~fn-name))

(defn message
  "Print a message to the echo area."
  [& strings]
  (let [msg (apply str strings)] ; or concatenate or format?
       (with-minibuffer m
                        (.Message m msg))
       msg))

;; Thanks, Joseph (@selfsame), for the repl environment tip!
(def repl-env (atom (arcadia.repl/env-map)))

(defn repl-eval-print-string
  "This repl is used by the eval-expression command.

  (def result-string (repl-eval-print-string \"(+ 1 2)\"))"
  [s]
  (let [{:keys [result env]} (arcadia.repl/repl-eval-print repl-env s)]
       (reset! repl-env env)
       result))
