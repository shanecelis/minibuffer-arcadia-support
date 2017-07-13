;; This file is a total mess of exploratory garbage programming.
(ns
 minibuffer.lisp.example
  (:use arcadia.core
        minibuffer.lisp.core
        clojure.pprint)
  (:import
   [UnityEngine Time Mathf Debug]
   [SeawispHunter.MinibufferConsole Minibuffer ]))

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

;; (pprint (macroexpand-1 '(defcmd ^Int64 ^{:key-binding "C-h f"} say-hello3 [^String x]
;;                    (message "Hi, %s!" x)
;;                    1)))

(defcmd ^{:key-binding "C-c 4"} say-hello4 [^String ^{:prompt "What's your name? "} x]
  (message "Hi, %s!" x))

(defcmd clojure-is
  [
  ^{:prompt "Clojure is... "
    :completions ["good" "the best!" "has too many parens"]
    :require-match true                ; This is required to force a match on RET.
    }
   ^String
   x]
;  (message "clojure-is %s" (pr-str x))
  (if-not (= x "has too many parens")
      (message "Yes, it is.")
    (message "Wrong. It's the best!")))

;; (pprint (macroexpand-1 '
;;          (defcmd clojure-is [^String ^{:prompt "Clojure is... " :completions ["good" "the best!" "has too many parens"]} x]
;;            (message x)
;;            (if-not (= x "has too many parens")
;;                    (message "Yes, it is.")
;;                    (message "Wrong. It's the best!")))))
;; (pprint (macroexpand-1 '(defcmd ^Int64 say-hello4 [^String ^{:prompt "What's your name? "} x]
;;                    (message "Hi, %s!" x)
;;                    1)))


;; (macroexpand-1 '(defcmd ^Int64 say-hello4 [^String ^{:prompt "What's your name? "} x]
;;    (message "Hi, %s!" x)
;;    1))



;; (macroexpand-1 '(defcmd ^Int64 say-hello4 [^String  x]
;;                   (message "Hi, %s!" x)
;;                   1))

;(def a (atom 0))
;(def b (atom 0))
;(defn b-get [] @b)
;(macroexpand '(defvar c "c be cool" 2))
(defvar a 0)
(defparam b 1)
(defvar c "c is a long variable." 2)
(defparam d "d is a long parameter." 3)

;(register-variable Int64 "a" (fn [] @a) (fn [v] (reset! a v)))
;(register-variable Int64 "b" (fn [] @b) (fn [v] (reset! b v)))
;;(macroexpand '(register-variable Int64 "b" (fn [] @b) (fn [v] (reset! b v))))
