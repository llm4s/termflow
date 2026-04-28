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
