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

  [Command("eval-expression", keySequence = "M-:")]
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
