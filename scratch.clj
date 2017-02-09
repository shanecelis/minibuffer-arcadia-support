;; This file is a total mess of exploratory garbage programming.
(ns
 minibuffer.lisp.scratch
  (:require arcadia.core)
  (:import
   [UnityEngine Time Mathf Debug]
   [seawisphunter.minibuffer Minibuffer Command]))

(import [UnityEngine Debug]
      [seawisphunter.minibuffer Minibuffer Command])
(defn my-inc [x] (+ x 1))

;;  #(float (* (Mathf/Cos UnityEngine.Time/time) 100))
;;  #(* (Mathf/Cos Time/time) 100)
(defn field-of-view [f]
  (hook+ UnityEngine.Camera/main :update #(set! (.fieldOfView (cmpt % UnityEngine.Camera)) (float (f)))))

;; Minibuffer.instance
;;   .RegisterCommand<int>(new Command("clojure-cmd"),
;;                         (int x) => Debug.Log("clojure-cmd run! " + x));
(defn add-test-command []
  (. Minibuffer/instance RegisterCommand2 (Command. "clojure-cmd")
     (sys-action [Int32] [x] (Debug/Log (format "clojure-cmd run! %d" x)))))

(. Minibuffer/instance RegisterCommand2 (Command. "clojure-cmd")
   (sys-action [Int32] [x] (Debug/Log (format "clojure-cmd run! %d" x))))

(defcmd ^Int32 macro-test "testing 1 2 3" [Int32 Int32] [x] (. Minibuffer/instance Message "Hi") x)
(defcmd ^Int32 macro-test "testing" [] [x] (. Minibuffer/instance Message "Hi") x)

(defcmd2 ^Int32 macro-test2 "testing" [Int32] [x] (. Minibuffer/instance Message "Hi") x)
(defcmd2 ^Int32 macro-test "testing" [Int32] [x] (. Minibuffer/instance Message "Hi") x)
(defcmd macro-test "testing" [Int32] [x] (. Minibuffer/instance Message "Hi") x)
(defcmd string-test "testing" [String] [x] x)
(defcmd2 macro-test32 "testing" [Int32] [x] (. Minibuffer/instance Message "Hi") x)
(defcmd noargs "testing" [] [] (. Minibuffer/instance Message "Hi"))
(defcmd ^Int32 noargsbutreturn "testing" [Int32] [] (. Minibuffer/instance Message "Hi") 1)
(macroexpand '(defcmd ^Int32 noargsbutreturn "testing" [] [] (. Minibuffer/instance Message "Hi") 1))
(macroexpand '(defcmd ^Int32 noargsbutreturn "testing" [] [] (. Minibuffer/instance Message "Hi") 1))
(defcmd noargs "testing" [] [] (. Minibuffer/instance Message "Hi"))
(macroexpand '(defcmd noargs "testing" [] [] (. Minibuffer/instance Message "Hi")))
(macroexpand '(defcmd ^Int32 macro-test "testing" [Int32 Int32] [x] (. Minibuffer/instance Message "Hi") x))

(defmacro defcmd2
    "hi"
  [fn-name & args]
  `(let [x# (meta #'~fn-name)]
       [x#]))

(defmacro defcmd4
    "Define a minibuffer command and function."
  [fn-name docstring typeargs args & body]
  (if (empty? typeargs)
      `(do (defn ~fn-name
             ~docstring
             ~args
             ~@body)
           (. Minibuffer/instance RegisterCommand (Command. (name '~fn-name))
              (if-let [tag# ((meta #'~fn-name) :tag)]
                  (sys-func [tag#] ~args
                            (~fn-name ~@args))
                  (gen-delegate Action [] (~fn-name))))
         ;#'~fn-name
         )
      `(do (defn ~fn-name
             ~docstring
             ~args
             ~@body)
           (. Minibuffer/instance RegisterCommand (Command. (name '~fn-name))
              (if ((meta #'~fn-name) :tag)
                  (sys-func (conj ~typeargs ((meta #'~fn-name) :tag)) ~args
                            (~fn-name ~@args))
                (sys-action ~typeargs ~args (~fn-name ~@args))))
         #'~fn-name)))

(defcmd say-hello "Say hello to x. Return a number." [String Int32]
  [x]
  (message "Hi, " x "!")
  1)

(defmacro defcmd0
    "Define a minibuffer command and function. The docstring is required.

e.g. (defcmd say-hello \"Says hello.\" [String Int32] [x] (message x) 1)"
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
          #'~fn-name)))

(defmacro defcmd3
    "Define a minibuffer command and function."
  [fn-name docstring typeargs args & body]
  `(do (defn ~fn-name
         ~docstring
         ~args
         ~@body)
       (. Minibuffer/instance RegisterCommand (Command. (name '~fn-name))
          (if (empty? ~typeargs)
              (gen-delegate Action [] (~fn-name))
            nil))
     #'~fn-name))
(. Minibuffer/instance RegisterCommand (Command. "clojure-cmd")
   (sys-action [Int32] [x] (Debug/Log (format "clojure-cmd run! %d" x))))

(defn remove-test-command []
  (. Minibuffer/instance UnregisterCommand "clojure-cmd"))


(add-test-command)

;; (let [ts (objects-named #".+")]
;;   (hook+ (first ts) :on-draw-gizmos
;;          (fn [o]
;;            (reduce #(do (Gizmos/DrawLine %1 %2) %2)
;;                    (map #(.. % transform position) ts)))))
