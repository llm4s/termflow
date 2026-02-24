package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.Tui.*

class TuiSpec extends AnyFunSuite {

  test("TermFlowError enum cases can be constructed and matched") {
    val errors = List[TermFlowError](
      TermFlowError.ConfigError("cfg"),
      TermFlowError.ModelNotFound,
      TermFlowError.Unexpected("boom"),
      TermFlowError.Validation("bad"),
      TermFlowError.CommandError("cmd"),
      TermFlowError.UnknownApp("name")
    )

    assert(errors.length == 6)
    assert(errors.exists {
      case TermFlowError.ModelNotFound => true
      case _                           => false
    })
  }

  test("Tui extension helpers lift model and command") {
    val model = 42
    assert(model.tui[String] == Tui(42, Cmd.NoCmd))
    assert(model.gCmd[String]("ok") == Tui(42, Cmd.GCmd("ok")))
  }
}
