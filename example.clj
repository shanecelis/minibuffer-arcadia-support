;; This file is a total mess of exploratory garbage programming.
(ns
 minibuffer.lisp.example
  (:use arcadia.core
        minibuffer.lisp.core
        clojure.pprint)
  (:import
   [UnityEngine Time Mathf Debug]
   [seawisphunter.minibuffer Minibuffer Command]))

(defcmd say-hello0 []
  (message "Hi!"))

(defcmd say-hello "Say hello to x. Return a number." [^String x]
  (message "Hi, " x "!")
  1)

;; no docstring
(defcmd say-hello2 [^String x]
  (message "Hi, " x "!")
  1)

(defcmd ^Int64 say-hello3 [^String x]
  (message "Hi, " x "!")
  1)

(pprint (macroexpand-1 '(defcmd ^Int64 say-hello3 [^String x]
                   (message "Hi, " x "!")
                   1)))

(defcmd ^Int64 say-hello4 [^String ^{:prompt "What's your name? "} x]
  (message "Hi, " x "!")
  1)

(defcmd clojure-is [^String ^{:prompt "Clojure is... " :completions ["good" "the best!" "has too many parens"]} x]
  (message x)
  (if-not (= x "has too many parens")
      (message "Yes, it is.")
    (message "Wrong. It's the best!")))

(pprint (macroexpand-1 '
         (defcmd clojure-is [^String ^{:prompt "Clojure is... " :completions ["good" "the best!" "has too many parens"]} x]
           (message x)
           (if-not (= x "has too many parens")
                   (message "Yes, it is.")
                   (message "Wrong. It's the best!")))))
(pprint (macroexpand-1 '(defcmd ^Int64 say-hello4 [^String ^{:prompt "What's your name? "} x]
                   (message "Hi, " x "!")
                   1)))


(macroexpand-1 '(defcmd ^Int64 say-hello4 [^String ^{:prompt "What's your name? "} x]
   (message "Hi, " x "!")
   1))



(macroexpand-1 '(defcmd ^Int64 say-hello4 [^String  x]
                  (message "Hi, " x "!")
                  1))
