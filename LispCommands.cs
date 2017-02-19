using UnityEngine;
using System.Collections;
using seawisphunter.minibuffer;
using clojure.lang;
using Arcadia;

[CommandGroup("lisp", autoRegister = false)]
public class LispCommands : MonoBehaviour {

  void Start() {
    RT.load("minibuffer/lisp/core");
    RT.load("minibuffer/lisp/example");

    var result = RT.var("minibuffer.lisp.core", "repl-setup")
      .invoke();

    //EvalExpression("(minibuffer.lisp.core/repl-setup)");
    // Should I do a (use 'minibuffer.lisp.core 'arcadia.core) here too?
    Minibuffer.OnStart((m) => m.Register(this));
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
    var result = RT.var("minibuffer.lisp.core", "repl-eval-print-string")
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
