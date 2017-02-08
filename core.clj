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
  (:use arcadia.core) ;; use or require?  I don't know the difference.
  (:import
   [UnityEngine Time Mathf Debug]
   [seawisphunter.minibuffer Minibuffer Command]))

(log "minibuffer.lisp.core reloaded")

;; (defn register-command
;;   ""
;;   ([cmd-name typeargs func]
;;              (if (fn? func)
;;                  (let [hash (meta (var func))
;;                       args (hash :arglists)
;;                       string-name (name cmd-name)]
;;                       (defcmd -name (hash :doc) typeargs args (fn args)))
;;                (throw (ArgumentException. "Must provide a function for fn.")))))

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
  (let [sys-fn (if (= (count typeargs) (+ 1 (count args)))
                   'sys-func
                 (if (= (count typeargs) (count args))
                     'sys-action
                   (throw (ArgumentException. "Typeargs and args must be equal length or typeargs may have one more element at the end to represent its return type."))))
       del     (if (empty? args)
                   `(gen-delegate Action [] (~fn-name))
                 `(~sys-fn ~typeargs ~args (~fn-name ~@args)))
       string-name (name fn-name)]
       `(do (defn ~fn-name
              ~docstring
              ~args
              ~@body)
            (. Minibuffer/instance RegisterCommand (doto (Command. ~string-name)
                                                         (.set_description ~docstring))
               ~del)
          #'~fn-name)) )

;;  #(float (* (Mathf/Cos UnityEngine.Time/time) 100))
;;  #(* (Mathf/Cos Time/time) 100)
(defn field-of-view [f]
  (hook+ UnityEngine.Camera/main
         :update #(set! (.fieldOfView (cmpt % UnityEngine.Camera)) (float (f)))))

(defn message
  "Print a message to the echo area."
  [& strings]
  (let [msg (apply str strings)]
       (. Minibuffer/instance Message msg)
       msg))

(defcmd say-hello
  "Say hello to x. Return a number."
  [String Int32]
  [x]
  (message "Hi, " x " from Arcadia!")
  2)

(defonce defer-count 0)

(defn do-with-minibuffer
  "Run (f Minibuffer/instance) when Minibuffer/instance is available.

   FIXME: I don't think this works currently."
  [f]
  (if (nil? Minibuffer/instance)
      (do
          (def defer-count (inc defer-count))
          (log "defering do-with-minibuffer")
          (Minibuffer/OnStart (gen-delegate Action [] (f Minibuffer/instance))) )
    (f Minibuffer/instance)))
