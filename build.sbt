import sbt._
import Keys._
import _root_.scalafix.sbt.{BuildInfo => ScalafixBuildInfo}
import sbtunidoc.ScalaUnidocPlugin
import sbtunidoc.BaseUnidocPlugin.autoImport.*
import sbtunidoc.ScalaUnidocPlugin.autoImport.*

val scala3   = Versions.scala3
val fixedLocalVersion = "0.1.1-SNAPSHOT"

inThisBuild(
  List(
    organization       := "org.llm4s",
    organizationName   := "llm4s",
    scalaVersion       := scala3,
    semanticdbEnabled  := true,
    semanticdbVersion  := scalafixSemanticdb.revision,
    versionScheme      := Some("early-semver"),
    homepage           := Some(url("https://github.com/llm4s/termflow")),
    licenses           := List("MIT" -> url("https://mit-license.org/")),
    developers := List(
      Developer(
        "rorygraves",
        "Rory Graves",
        "rory.graves@fieldmark.co.uk",
        url("https://github.com/rorygraves")
      )
    ),
    // Publish to Sonatype Central Portal via staging
    publishTo := {
      val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
      if (isSnapshot.value) Some("central-snapshots".at(centralSnapshots))
      else localStaging.value
    },
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toArray),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/llm4s/termflow/"),
        "scm:git:git@github.com:llm4s/termflow.git"
      )
    ),
    version := {
      dynverGitDescribeOutput.value match {
        case Some(out) if !out.isSnapshot() =>
          out.ref.value.stripPrefix("v")
        case Some(out) =>
          val baseVersion = out.ref.value.stripPrefix("v")
          s"$baseVersion+${out.commitSuffix.mkString("", "", "")}-SNAPSHOT"
        case None =>
          "0.0.0-UNKNOWN"
      }
    }
  )
)

lazy val commonSettings = Seq(
  // Scala 3 compiler option reference:
  // https://docs.scala-lang.org/scala3/guides/migration/options-intro.html
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Wvalue-discard",  // warn when non-Unit results are ignored
    "-Wunused:all", // surface dead code/imports during Scala 3 cleanup
    "-Wsafe-init", // flag potentially unsafe field initialization order
    "-source:3.7-migration" // emit migration guidance without failing builds
  ),
  Test / fork := true,
  // Forward the golden-update flag into forked test JVMs so
  // `sbt -Dtermflow.update-goldens=true test` works end-to-end.
  Test / javaOptions ++= sys.props
    .get("termflow.update-goldens")
    .toSeq
    .map(v => s"-Dtermflow.update-goldens=$v")
)

lazy val scalafixRuleDependencies = Def.setting {
  Seq(
    ("ch.epfl.scala" %% "scalafix-core" % ScalafixBuildInfo.scalafixVersion)
      .cross(CrossVersion.for3Use2_13) % ScalafixConfig
  ) ++
    (if (scalaBinaryVersion.value == "3")
       Seq("org.scala-lang" %% "scala3-library" % scalaVersion.value % ScalafixConfig)
     else Nil)
}

// Internal-only project hosting the custom scalafix rules
// (NoDirectRuntimeConfigAccess, NoDirectTerminalIO). Compiled against
// scalafix-core 2.13 (since scalafix runs on Scala 2.13 internally) and
// pulled into each framework module's scalafix classpath via
// `dependsOn(... % ScalafixConfig)` below.
lazy val termflowScalafixRules = (project in file("modules/termflow-scalafix-rules"))
  .settings(
    name           := "termflow-scalafix-rules",
    scalaVersion   := scala3,
    publish / skip := true,
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Wunused:imports"),
    libraryDependencies ++= Seq(
      ("ch.epfl.scala" %% "scalafix-core" % ScalafixBuildInfo.scalafixVersion)
        .cross(CrossVersion.for3Use2_13)
    )
  )

// Settings shared by every framework module — scalafix custom rules
// configuration. Each framework module adds `termflowScalafixRules` as a
// `% ScalafixConfig` dependency at the project definition site so the
// rule JAR is on the scalafix classpath when running on that module.
lazy val frameworkScalafixSettings = Seq(
  scalafixConfig := Some((ThisBuild / baseDirectory).value / ".scalafix-termflow.conf"),
  libraryDependencies ++= scalafixRuleDependencies.value
)

lazy val root = (project in file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(
    termflowScalafixRules,
    termflowTerminal,
    termflowScreen,
    termflowApp,
    termflowWidgets,
    termflow,
    termflowTestkit,
    termflowSample
  )
  .settings(
    name           := "termflow",
    publish / skip := true,
    // Aggregated Scaladoc for the four published framework modules. The
    // generated tree is consumed by the docs site workflow and copied into
    // the mdBook output under `/api/`.
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
      termflowTerminal,
      termflowScreen,
      termflowApp,
      termflowWidgets
    ),
    ScalaUnidoc / unidoc / target := (ThisBuild / baseDirectory).value / "target" / "unidoc"
  )

// ---- Layered framework modules -------------------------------------------
// terminal -> screen -> app -> widgets. Each carries its share of the
// `termflow.tui` package; they end up on the same classpath via the
// transitive deps but stay separately publishable so apps can depend on a
// thin slice when rolling their own widgets.

lazy val termflowTerminal = (project in file("modules/termflow-terminal"))
  .dependsOn(termflowScalafixRules % ScalafixConfig)
  .settings(
    name := "termflow-terminal",
    description :=
      "Terminal layer: backend trait + JLine impl, key decoding, capabilities, mouse, unicode width.",
    commonSettings,
    frameworkScalafixSettings,
    libraryDependencies ++= Seq(
      Deps.jline,
      Deps.scalatest % Test
    )
  )

lazy val termflowScreen = (project in file("modules/termflow-screen"))
  .dependsOn(termflowTerminal, termflowScalafixRules % ScalafixConfig)
  .settings(
    name := "termflow-screen",
    description :=
      "Screen layer: VNode / Layout / Overlay, ANSI renderer, RenderFrame, themes, border chars.",
    commonSettings,
    frameworkScalafixSettings,
    libraryDependencies ++= Seq(
      Deps.scalatest               % Test,
      Deps.scalacheck              % Test,
      Deps.scalatestPlusScalacheck % Test
    )
  )

lazy val termflowApp = (project in file("modules/termflow-app"))
  .dependsOn(termflowTerminal, termflowScreen, termflowScalafixRules % ScalafixConfig)
  .settings(
    name := "termflow-app",
    description :=
      "Application layer: TuiApp, Tui, Cmd, Sub, runtime, Dialogs, focus, keymap, devtools, prompt history.",
    commonSettings,
    frameworkScalafixSettings,
    libraryDependencies ++= Seq(
      Deps.jline,
      Deps.pureconfigCore,
      Deps.pureconfigGenericScala3,
      Deps.scalatest % Test
    )
  )

lazy val termflowWidgets = (project in file("modules/termflow-widgets"))
  .dependsOn(termflowApp, termflowScalafixRules % ScalafixConfig)
  .settings(
    name := "termflow-widgets",
    description :=
      "Widget catalogue: Button, TextField, ListView, Table, Select, Autocomplete, MenuBar, Form, SplitPane, Tabs, Tree, etc.",
    commonSettings,
    frameworkScalafixSettings,
    libraryDependencies ++= Seq(
      Deps.scalatest % Test
    )
  )

// Umbrella artifact `termflow` — pre-1.0 transitional shim so existing
// `org.llm4s:termflow_3` deps still resolve to the full set of framework
// modules. The directory carries no Scala sources of its own; it only
// hosts the custom scalafix rules consumed by the four framework modules
// above (under `src/scalafix/scala`). The published JAR is empty; the POM
// lists the four modules as transitive deps so users get the same surface.
//
// `frameworkScalafixSettings` is repeated here so the scalafix rule
// sources under `src/scalafix/scala` compile against scalafix-core when
// sbt-scalafix invokes them — without it, scalafix invocations on the
// dependent modules fail with "Could not find package scala from compiler
// core libraries" because the custom-rules project never resolves the
// Scala stdlib for its scalafix classpath.
lazy val termflow = (project in file("modules/termflow"))
  .dependsOn(termflowTerminal, termflowScreen, termflowApp, termflowWidgets)
  .settings(
    name        := "termflow",
    description := "A small, functional terminal UI (TUI) framework for Scala (umbrella artifact).",
    commonSettings
  )

lazy val termflowTestkit = (project in file("modules/termflow-testkit"))
  .dependsOn(termflowApp, termflowWidgets)
  .settings(
    name := "termflow-testkit",
    description :=
      "Deterministic test harness for TermFlow apps: TuiTestDriver, golden snapshots, virtual runtime context.",
    commonSettings,
    // ScalaTest is a compile-scope dep here because GoldenSupport mixes Suite into
    // consumer test classes — they need scalatest types on the classpath at test time.
    libraryDependencies ++= Seq(
      Deps.scalatest
    )
  )

lazy val termflowSample = (project in file("modules/termflow-sample"))
  .dependsOn(
    termflow                          % "compile->compile;test->test",
    termflowTestkit                   % Test
  )
  .settings(
    name := "termflow-sample",
    commonSettings,
    libraryDependencies ++= Seq(
      Deps.jline,
      Deps.scalatest % Test
    ),
    coverageEnabled := false,
    publish / skip  := true
  )

addCommandAlias("ciCheck", ";scalafmtCheckAll;scalafixAll --check;test")
// `coverage`/`coverageReport` are sbt-scoverage *commands* — they
// can't be invoked with the `project/key` selector syntax. The pattern
// below switches into each framework module in turn, enables coverage,
// runs its tests, and emits the per-module scoverage report. CI picks
// up the resulting `modules/termflow-*/target/scala-*/scoverage-report/`
// directories.
addCommandAlias(
  "coverageLib",
  ";project termflowTerminal;coverage;test;coverageReport" +
    ";project termflowScreen;coverage;test;coverageReport" +
    ";project termflowApp;coverage;test;coverageReport" +
    ";project termflowWidgets;coverage;test;coverageReport" +
    ";project root"
)
addCommandAlias("prePR", ";ciCheck;coverageLib;termflowSample/runMain termflow.run.SampleSmoke")
addCommandAlias(
  "publishLocalFixed",
  s"""set ThisBuild / version := "$fixedLocalVersion"; publishLocal"""
)

// --- Sample-app launchers --------------------------------------------------
// Short aliases so contributors can run a demo without typing the full
// runMain incantation. Use a real interactive terminal — JLine falls back
// to a dumb backend when stdin/stdout are pipes.
addCommandAlias("hubDemo",      "termflowSample/runMain termflow.run.SampleHubMain")
addCommandAlias("widgetsDemo",  "termflowSample/runMain termflow.apps.widgets.WidgetsDemoApp")
addCommandAlias("formDemo",     "termflowSample/runMain termflow.apps.forms.FormDemoApp")
addCommandAlias("catalogDemo",  "termflowSample/runMain termflow.apps.catalog.CatalogDemoApp")
addCommandAlias("echoDemo",     "termflowSample/runMain termflow.apps.echo.EchoApp")
addCommandAlias("counterDemo",  "termflowSample/runMain termflow.apps.counter.SyncCounter")
addCommandAlias("futureDemo",   "termflowSample/runMain termflow.apps.counter.FutureCounter")
addCommandAlias("clockDemo",    "termflowSample/runMain termflow.apps.clock.DigitalClock")
addCommandAlias("tabsDemo",     "termflowSample/runMain termflow.apps.tabs.TabsDemoApp")
addCommandAlias("taskDemo",     "termflowSample/runMain termflow.apps.task.Task")
addCommandAlias("stressDemo",   "termflowSample/runMain termflow.apps.stress.RenderStressApp")
addCommandAlias("sineDemo",     "termflowSample/runMain termflow.apps.stress.SineWaveApp")
addCommandAlias("inputDemo",    "termflowSample/runMain termflow.apps.input.InputLineReproApp")
addCommandAlias("themeDemo",    "termflowSample/runMain termflow.apps.themes.ThemeDemoApp")
addCommandAlias("dialogDemo",   "termflowSample/runMain termflow.apps.dialog.DialogDemoApp")
addCommandAlias("showcase",     "termflowSample/runMain termflow.apps.showcase.Stage1ShowcaseApp")
addCommandAlias("unicodeDemo",  "termflowSample/runMain termflow.apps.unicode.UnicodeDemoApp")
addCommandAlias("dashboardDemo","termflowSample/runMain termflow.apps.dashboard.DashboardApp")
addCommandAlias("wizardDemo",   "termflowSample/runMain termflow.apps.wizard.WizardApp")
addCommandAlias("treeDemo",     "termflowSample/runMain termflow.apps.tree.FileTreeApp")
addCommandAlias("editorDemo",   "termflowSample/runMain termflow.apps.editor.EditorApp")
addCommandAlias("chatDemo",     "termflowSample/runMain termflow.apps.chat.ChatStreamApp")

addCommandAlias("multilinePromptDemo", "termflowSample/runMain termflow.apps.multiline.MultilinePromptDemo")
addCommandAlias("unicodeInputDemo", "termflowSample/runMain termflow.apps.unicode.UnicodeInputDemo")
