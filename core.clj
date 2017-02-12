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

(log "minibuffer.lisp.core reloaded")

;; This is just me trying to debug do-with-minibuffer
(defonce defer-count 0)

(defn do-with-minibuffer
  "Run (f Minibuffer/instance) when Minibuffer/instance is available.

   FIXME: I don't think this works currently."
  [f]
  (if (nil? Minibuffer/instance)
      (do
          (def defer-count (inc defer-count))
          (log "defering with-minibuffer")
        (Minibuffer/OnStart (gen-delegate Action [] (f Minibuffer/instance))) )
    (f Minibuffer/instance)))

(defmacro with-minibuffer
    [minibuffer-var & body]
    `(do-with-minibuffer (fn [~minibuffer-var] ~@body)))

(defmacro make-delegate
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

(defmacro defcmd
    "Define a Minibuffer command and function. The docstring is required.
  The typeargs define what C# types the function accepts and optionally what it
  returns. Minibuffer makes extensive use of the type information. Consider the
  following example.

  e.g. (defcmd say-hello \"Says hello. Return int.\" [String Int32]
          [x]
          (message \"Hello, \" x)
          1)

  The function `say-hello` be defined as if it were a normal `defn` but it may
  additionally be run interactively by typing `M-x say-hello` which will then
  prompt for a string. Functions with a String output will be `message`'d
  automatically. Functions with a IEnumerator output will be run as coroutines."
  [fn-name docstring typeargs args & body]
  `(do (defn ~fn-name
         ~docstring
         ~args ;; add type annotation?
         ~@body)
       (with-minibuffer ~'m
                        (.RegisterCommand ~'m (doto (Command. ~(name fn-name))
                                                    (.set_description ~docstring))
                                          (make-delegate ~typeargs ~args ~fn-name)))
     #'~fn-name))

(defmacro separate-types [f args]
          `{:typeargs (->> '~args
                           (mapv meta)
                           (mapv :tag))
          :typeresult (:tag (meta '~f))})

;; (defmacro separate-types-oldform [f args]
;;           `(let [{:keys [typeargs# typeresult#]} (separate-types ~f ~args)]
;;                 (if typeresult#
;;                     (conj typeargs# typeresult#)
;;                   typeargs#)))

(defmacro separate-types-oldform [f args]
          `(let [h# (separate-types ~f ~args)]
                (if (:typeresult h#)
                    (conj (:typeargs h#) (:typeresult h#))
                  (:typeargs h#))))
(defn register-command
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

(defmacro defcmd2
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
  [& strings] ;; or concatenate or format?
  (let [msg (apply str strings)]
       (with-minibuffer m
                        (.Message m msg))
       msg))

;; Thanks, Joseph (@selfsame), for the repl environment tip!
(def repl-env (atom (arcadia.repl/env-map)))

(defn repl-eval-print-string [s]
  (let [{:keys [result env]} (arcadia.repl/repl-eval-print repl-env s)]
       (reset! repl-env env)
       result))
