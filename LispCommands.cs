using UnityEngine;
using System.Collections;
using SeawispHunter.MinibufferConsole;
using SeawispHunter.MinibufferConsole.Extensions;
using clojure.lang;
using Arcadia;

[Group("lisp")]
public class LispCommands : MonoBehaviour {

  public const string defaultInitCode
    = "(in-ns 'user)\n"
    + "(use 'minibuffer.lisp.core 'arcadia.core 'clojure.repl)\n"
    + "(import [UnityEngine Time Mathf Debug])";
  [TextArea]
  // public string initCode2 = defaultInitCode;
  public string initCode = defaultInitCode;
  public MinibufferListing minibufferExtensions;

  void Start() {
    if (initCode.IsZull())
      initCode = defaultInitCode;
    try {
      RT.load("minibuffer/lisp/core");

      RT.var("minibuffer.lisp.core", "repl-setup")
        .invoke();
    } catch (System.Exception e) {
      Debug.LogWarning("Error loading minibuffer.lisp.core.");
      Debug.LogException(e);
    }
    EvalExpression("(do " + initCode + ")");
    Minibuffer.Register(this);
  }

  /**
    This could also be implemented with a clojure function.

   ```
   (defcmd eval-expression
     "Evaluate an expression and show its result in the echo-area."
     [^{:prompt "Eval: " :history "expression" :key-binding "M-;"}
      ^String expression]

     (message (trim (repl-eval-print-string expression))))
   ```
   */
  [Command("eval-expression", keyBinding = "M-:",
           description = "Evaluates a Clojure expression and shows result.")]
  public void EvalExpression([Prompt("Eval: ",
                                     history = "expression")]
                             string expression) {
    var result = RT.var("minibuffer.lisp.internal", "repl-eval-print-string")
      .invoke(expression)
      .ToString()
      .Trim();
    Minibuffer.instance.MessageOrBuffer("*eval*", result);
  }


  // [Command("describe-function", keyBinding = "C-h f",
  //          description = "Show documentation for a Clojure function.")]
  // public void DescribeFunction([Prompt("Describe function: ",
  //                                      history = "function")]
  //                               string function) {
  //   // This was actually easier to write in Clojure.
  //   var result = RT.var("minibuffer.lisp.core", "doc-fn")
  //     .invoke(RT.var("clojure.core", "symbol").invoke(function))
  //     .ToString()
  //     .Trim();
  //   Minibuffer.instance.Message(result);
  // }
}
