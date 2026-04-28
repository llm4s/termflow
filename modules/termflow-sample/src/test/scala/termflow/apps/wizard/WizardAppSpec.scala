package termflow.apps.wizard

import org.scalatest.funsuite.AnyFunSuite
import termflow.testkit.TuiTestDriver
import termflow.tui.KeyDecoder.InputKey

class WizardAppSpec extends AnyFunSuite:

  private def driver: TuiTestDriver[WizardApp.Model, WizardApp.Msg] =
    val d = TuiTestDriver(WizardApp.App, width = 80, height = 24)
    d.init()
    d

  // ---- step navigation -----------------------------------------------------

  test("starts on Account step with NameId focused") {
    val d = driver
    assert(d.model.step == WizardApp.Step.Account)
    assert(d.model.currentFocus.current.contains(WizardApp.NameId))
  }

  test("can't advance past Account step with empty fields — errors populated") {
    val d = driver
    d.send(WizardApp.Msg.NextStep)
    assert(d.model.step == WizardApp.Step.Account, "must stay on Account when invalid")
    assert(d.model.errors.contains(WizardApp.NameId.value))
    assert(d.model.errors.contains(WizardApp.EmailId.value))
  }

  test("invalid email surfaces a focused error") {
    val d = driver
    typeText(d, "Alice")
    // Move to email field and type a value without '@'.
    d.send(WizardApp.Msg.NextFocus)
    typeText(d, "not-an-email")
    d.send(WizardApp.Msg.NextStep)
    assert(d.model.step == WizardApp.Step.Account)
    assert(d.model.errors.get(WizardApp.EmailId.value).contains("Email must contain '@'"))
    assert(!d.model.errors.contains(WizardApp.NameId.value))
  }

  test("Account → Plan with both fields valid") {
    val d = driver
    typeText(d, "Alice")
    d.send(WizardApp.Msg.NextFocus)
    typeText(d, "alice@example.com")
    d.send(WizardApp.Msg.NextStep)
    assert(d.model.step == WizardApp.Step.Plan)
    assert(d.model.errors.isEmpty)
  }

  test("Plan ↑/↓ moves the radio selection") {
    val d = atPlanStep
    assert(d.model.planIndex == 0)
    d.send(WizardApp.Msg.PlanDown)
    assert(d.model.planIndex == 1)
    d.send(WizardApp.Msg.PlanDown)
    assert(d.model.planIndex == 2)
    // Clamped at the end.
    d.send(WizardApp.Msg.PlanDown)
    assert(d.model.planIndex == 2)
    d.send(WizardApp.Msg.PlanUp)
    assert(d.model.planIndex == 1)
  }

  test("Plan → Confirm moves forward; Confirm → Plan moves back") {
    val d = atPlanStep
    d.send(WizardApp.Msg.NextStep)
    assert(d.model.step == WizardApp.Step.Confirm)
    d.send(WizardApp.Msg.PrevStep)
    assert(d.model.step == WizardApp.Step.Plan)
  }

  test("Account back is a no-op (already on first step)") {
    val d = driver
    d.send(WizardApp.Msg.PrevStep)
    assert(d.model.step == WizardApp.Step.Account)
  }

  // ---- focus / activation --------------------------------------------------

  test("Tab cycles focus inside the Account step") {
    val d = driver
    assert(d.model.currentFocus.current.contains(WizardApp.NameId))
    d.send(WizardApp.Msg.Key(InputKey.Tab))
    assert(d.model.currentFocus.current.contains(WizardApp.EmailId))
    d.send(WizardApp.Msg.Key(InputKey.Tab))
    assert(d.model.currentFocus.current.contains(WizardApp.NextAccountId))
    // Wraps around.
    d.send(WizardApp.Msg.Key(InputKey.Tab))
    assert(d.model.currentFocus.current.contains(WizardApp.NameId))
  }

  test("Enter on the Next button submits Account → Plan when valid") {
    val d = driver
    typeText(d, "Alice")
    d.send(WizardApp.Msg.NextFocus)
    typeText(d, "alice@example.com")
    d.send(WizardApp.Msg.NextFocus) // focus the Next button
    assert(d.model.currentFocus.current.contains(WizardApp.NextAccountId))
    d.send(WizardApp.Msg.Key(InputKey.Enter))
    assert(d.model.step == WizardApp.Step.Plan)
  }

  // ---- confirm step + submit -----------------------------------------------

  test("summary on the Confirm step reflects entered values") {
    val d       = atConfirmStep
    val summary = d.model.summary
    assert(summary.exists(_.contains("Alice")))
    assert(summary.exists(_.contains("alice@example.com")))
    assert(summary.exists(_.contains("Pro"))) // we move planIndex to 1
  }

  test("Submit flips submitted = true; submission text appears in the rendered frame") {
    val d = atConfirmStep
    // Focus the Submit button (Confirm focus order: Back, Submit).
    d.send(WizardApp.Msg.NextFocus)
    assert(d.model.currentFocus.current.contains(WizardApp.SubmitId))
    d.send(WizardApp.Msg.Submitted)
    assert(d.model.submitted)
    val rendered = renderedFrame(d)
    assert(rendered.contains("Submitted"))
  }

  // ---- quitting ------------------------------------------------------------

  test("'q' quits when not focused on a TextField") {
    val d = atPlanStep
    d.send(WizardApp.Msg.Key(InputKey.CharKey('q')))
    assert(d.exited)
  }

  test("'q' is treated as a normal character inside a TextField") {
    val d = driver
    d.send(WizardApp.Msg.Key(InputKey.CharKey('q')))
    assert(!d.exited, "q inside the name field should not quit the wizard")
    assert(d.model.name.buffer == "q")
  }

  // ---- helpers -------------------------------------------------------------

  private def typeText(d: TuiTestDriver[WizardApp.Model, WizardApp.Msg], s: String): Unit =
    s.foreach(c => d.send(WizardApp.Msg.Key(InputKey.CharKey(c))))

  private def atPlanStep: TuiTestDriver[WizardApp.Model, WizardApp.Msg] =
    val d = driver
    typeText(d, "Alice")
    d.send(WizardApp.Msg.NextFocus)
    typeText(d, "alice@example.com")
    d.send(WizardApp.Msg.NextStep)
    d

  private def atConfirmStep: TuiTestDriver[WizardApp.Model, WizardApp.Msg] =
    val d = atPlanStep
    d.send(WizardApp.Msg.PlanDown) // 0 → 1 (Pro)
    d.send(WizardApp.Msg.NextStep)
    d

  private def renderedFrame(d: TuiTestDriver[WizardApp.Model, WizardApp.Msg]): String =
    val frame = d.frame
    (0 until frame.height).map(r => frame.cells(r).map(_.ch).mkString).mkString("\n")
