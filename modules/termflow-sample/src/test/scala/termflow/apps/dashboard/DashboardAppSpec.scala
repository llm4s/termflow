package termflow.apps.dashboard

import org.scalatest.funsuite.AnyFunSuite
import termflow.testkit.TuiTestDriver
import termflow.tui.KeyDecoder.InputKey

class DashboardAppSpec extends AnyFunSuite:

  private def driver: TuiTestDriver[DashboardApp.Model, DashboardApp.Msg] =
    val d = TuiTestDriver(DashboardApp.App, width = 100, height = 28)
    d.init()
    d

  test("initial frame renders without exceptions and lists the default services") {
    val d = driver
    assert(d.model.services.size == DashboardApp.defaultServices.size)
    assert(d.model.tick == 0L)
    assert(!d.model.paused)
    val rendered = renderedFrame(d)
    DashboardApp.defaultServices.foreach(name =>
      assert(rendered.contains(name), s"expected service '$name' to appear in the frame")
    )
    assert(rendered.contains("TermFlow Dashboard"))
  }

  test("Tick advances the counter and mutates CPU values") {
    val d         = driver
    val cpuBefore = d.model.services.items.map(_.cpu)
    d.send(DashboardApp.Msg.Tick)
    assert(d.model.tick == 1L)
    val cpuAfter = d.model.services.items.map(_.cpu)
    // At least one value should change — the rng per-service delta is
    // [-0.10, +0.10) so the chance of every service rolling exactly 0
    // is infinitesimal. Guard against a stuck simulation.
    assert(cpuAfter != cpuBefore, "tick should perturb at least one CPU value")
    // All values stay clamped into [0, 1].
    assert(cpuAfter.forall(c => c >= 0.0 && c <= 1.0))
  }

  test("Tick increments per-service tick count") {
    val d = driver
    d.send(DashboardApp.Msg.Tick)
    d.send(DashboardApp.Msg.Tick)
    assert(d.model.services.items.forall(_.ticks == 2L))
  }

  test("ArrowDown moves the ListView selection") {
    val d = driver
    assert(d.model.services.selected == 0)
    d.send(DashboardApp.Msg.Key(InputKey.ArrowDown))
    assert(d.model.services.selected == 1)
    d.send(DashboardApp.Msg.Key(InputKey.ArrowDown))
    assert(d.model.services.selected == 2)
    d.send(DashboardApp.Msg.Key(InputKey.ArrowUp))
    assert(d.model.services.selected == 1)
  }

  test("'r' restarts the selected service — CPU drops to 0 and ticks reset") {
    val d = driver
    // Run the simulation a few ticks so a non-zero CPU exists to drop.
    (1 to 5).foreach(_ => d.send(DashboardApp.Msg.Tick))
    d.send(DashboardApp.Msg.Key(InputKey.ArrowDown)) // pick service[1]
    d.send(DashboardApp.Msg.Key(InputKey.CharKey('r')))
    val picked = d.model.services.items(1)
    assert(picked.cpu == 0.0, "selected service's CPU must be reset")
    assert(picked.ticks == 0L, "selected service's tick count must be reset")
    // Other services keep ticking.
    val others = d.model.services.items.zipWithIndex.collect { case (s, i) if i != 1 => s }
    assert(others.forall(_.ticks == 5L))
  }

  test("'p' toggles paused — Tick stops perturbing CPU values while paused") {
    val d = driver
    d.send(DashboardApp.Msg.Key(InputKey.CharKey('p')))
    assert(d.model.paused)
    val frozen = d.model.services.items
    d.send(DashboardApp.Msg.Tick)
    d.send(DashboardApp.Msg.Tick)
    assert(d.model.tick == 2L, "tick counter advances even while paused")
    assert(d.model.services.items == frozen, "CPU values must not move while paused")
    // Resume and verify ticking resumes.
    d.send(DashboardApp.Msg.Key(InputKey.CharKey('p')))
    assert(!d.model.paused)
    d.send(DashboardApp.Msg.Tick)
    assert(d.model.services.items != frozen, "tick after resume should perturb values")
  }

  test("Space also toggles pause") {
    val d = driver
    d.send(DashboardApp.Msg.Key(InputKey.CharKey(' ')))
    assert(d.model.paused)
  }

  test("'q' emits Cmd.Exit") {
    val d = driver
    d.send(DashboardApp.Msg.Key(InputKey.CharKey('q')))
    assert(d.exited, "'q' should request runtime exit")
  }

  test("Esc emits Cmd.Exit") {
    val d = driver
    d.send(DashboardApp.Msg.Key(InputKey.Escape))
    assert(d.exited)
  }

  test("hotCount counts services above the warning threshold") {
    val d = driver
    // Mutate the model directly to put two services into hot territory.
    val items = d.model.services.items
    val hot   = items.updated(0, items(0).copy(cpu = 0.95)).updated(2, items(2).copy(cpu = 0.90))
    val nm    = d.model.copy(services = d.model.services.withItems(hot))
    assert(nm.hotCount == 2)
  }

  test("StatusBar reports the live service / hot / tick summary") {
    val d = driver
    (1 to 3).foreach(_ => d.send(DashboardApp.Msg.Tick))
    val rendered = renderedFrame(d)
    assert(rendered.contains(s"${d.model.services.size} services"))
    assert(rendered.contains("tick=3"))
  }

  private def renderedFrame(d: TuiTestDriver[DashboardApp.Model, DashboardApp.Msg]): String =
    val frame = d.frame
    (0 until frame.height).map(r => frame.cells(r).map(_.ch).mkString).mkString("\n")
