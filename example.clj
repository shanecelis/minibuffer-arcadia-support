;; This file is a total mess of exploratory garbage programming.
(ns
 minibuffer.lisp.example
  (:use arcadia.core
        minibuffer.lisp.core
        clojure.pprint)
  (:import
   [UnityEngine Time Mathf Debug]
   [seawisphunter.minibuffer Minibuffer ]))

;; most basic
(defcmd say-hello0 []
  (message "Hi!"))

;; has argument
(defcmd say-hello1 [^String x]
  (message "Hi, %s!" x))

;; docstring
(defcmd say-hello2 "Say hello to x. Return a number." [^String x]
  (message "Hi, %s!" x))

;; returns int
(defcmd ^Int64 say-hello3 [^String x]
  (message "Hi, %s!" x)
  1)

(pprint (macroexpand-1 '(defcmd ^Int64 ^{:key-binding "C-h f"} say-hello3 [^String x]
                   (message "Hi, %s!" x)
                   1)))

(defcmd say-hello4 [^String ^{:prompt "What's your name? "} x]
  (message "Hi, %s!" x))

(defcmd clojure-is
  [
  ^{:prompt "Clojure is... "
  :completions ["good" "the best!" "has too many parens"]
  :require-match true                ; This is required to force a match on RET.
  }
  ^String
  x]
  (message "clojure-is %s" (pr-str x))
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
                   (message "Hi, %s!" x)
                   1)))


(macroexpand-1 '(defcmd ^Int64 say-hello4 [^String ^{:prompt "What's your name? "} x]
   (message "Hi, %s!" x)
   1))



(macroexpand-1 '(defcmd ^Int64 say-hello4 [^String  x]
                  (message "Hi, %s!" x)
                  1))
