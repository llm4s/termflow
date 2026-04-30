package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import pureconfig.ConfigSource

class TermFlowConfigSpec extends AnyFunSuite:
  test("metrics env override keeps legacy truthy semantics for 1"):
    val loaded = TermFlowConfigLoader.loadFrom(
      ConfigSource
        .string("""
          |termflow {
          |  logging {
          |    path = "/tmp/termflow.log"
          |  }
          |  metrics {
          |    enabled = false
          |    env-override = "1"
          |  }
          |}
          |""".stripMargin)
        .at("termflow")
    )

    assert(loaded.isSuccess)
    assert(loaded.get.metrics.enabled)

  test("metrics config still respects explicit boolean when no env override is present"):
    val loaded = TermFlowConfigLoader.loadFrom(
      ConfigSource
        .string("""
          |termflow {
          |  logging {
          |    path = "/tmp/termflow.log"
          |  }
          |  metrics {
          |    enabled = true
          |  }
          |}
          |""".stripMargin)
        .at("termflow")
    )

    assert(loaded.isSuccess)
    assert(loaded.get.metrics.enabled)

  test("accessibility.reducedMotion defaults to false when neither config nor env are set"):
    val loaded = TermFlowConfigLoader.loadFrom(
      ConfigSource
        .string("""
          |termflow {
          |  logging { path = "/tmp/termflow.log" }
          |  metrics { enabled = false }
          |  accessibility { reduced-motion = false }
          |}
          |""".stripMargin)
        .at("termflow")
    )

    assert(loaded.isSuccess)
    assert(!loaded.get.accessibility.reducedMotion)

  test("accessibility env-override flips reducedMotion on for truthy values"):
    val loaded = TermFlowConfigLoader.loadFrom(
      ConfigSource
        .string("""
          |termflow {
          |  logging { path = "/tmp/termflow.log" }
          |  metrics { enabled = false }
          |  accessibility {
          |    reduced-motion = false
          |    env-override = "1"
          |  }
          |}
          |""".stripMargin)
        .at("termflow")
    )

    assert(loaded.isSuccess)
    assert(loaded.get.accessibility.reducedMotion)

  test("accessibility env-override of \"0\" or \"false\" leaves reducedMotion off"):
    val zero = TermFlowConfigLoader.loadFrom(
      ConfigSource
        .string("""
          |termflow {
          |  logging { path = "/tmp/termflow.log" }
          |  metrics { enabled = false }
          |  accessibility {
          |    reduced-motion = true
          |    env-override = "0"
          |  }
          |}
          |""".stripMargin)
        .at("termflow")
    )

    assert(zero.isSuccess)
    assert(!zero.get.accessibility.reducedMotion)

  test("accessibility config respects explicit reduced-motion = true when env unset"):
    val loaded = TermFlowConfigLoader.loadFrom(
      ConfigSource
        .string("""
          |termflow {
          |  logging { path = "/tmp/termflow.log" }
          |  metrics { enabled = false }
          |  accessibility { reduced-motion = true }
          |}
          |""".stripMargin)
        .at("termflow")
    )

    assert(loaded.isSuccess)
    assert(loaded.get.accessibility.reducedMotion)
