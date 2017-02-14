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
    Minibuffer.OnStart((m) => m.Register(this));
  }

  /**
    This could also be implemented with a clojure function. 

   ```
   (defcmd eval-expression
     "Evaluate an expression and show its result in the echo-area."
     [^{:prompt "Eval: " :history "expression" :key-sequence "M-;"}
      ^String expression]

     (message (trim (repl-eval-print-string expression))))

   ```
   */
  [Command("eval-expression", keySequence = "M-:",
           description = "Evaluates a Clojure expression and shows result.")]
  public void EvalExpression([Prompt("Eval: ",
                                     history = "expression")]
                             string expression) {
    var result = RT.var("minibuffer.lisp.core", "repl-eval-print-string")
      .invoke(expression)
      .ToString()
      .Trim();
    Minibuffer.instance.Message(result);
  }
}
