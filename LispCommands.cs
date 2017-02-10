using UnityEngine;
using System.Collections;
using seawisphunter.minibuffer;
using clojure.lang;
using Arcadia;

[CommandGroup("lisp", autoRegister = false)]
public class LispCommands : MonoBehaviour {

	// Use this for initialization
	// void Start () {
  //   Minibuffer.OnStart(() => {
  //       Minibuffer.instance.GetKeymap("user")["M-:"] = "eval-expression";
  //     });
	// }
  void Start() {
    RT.load("minibuffer/lisp/core");
    Minibuffer.OnStart((m) => m.Register(this));
  }

  [Command("eval-expression", keySequence = "M-:")]
  public void EvalExpression([Prompt("Eval: ",
                                     history = "expression")]
                             string expression) {
    // FIXME: I should use a real environment.
    var result = RT.var("minibuffer.lisp.core", "repl-eval-print-string")
      .invoke(expression)
      .ToString()
      .Trim();
    Minibuffer.instance.Message(result);
  }
}
