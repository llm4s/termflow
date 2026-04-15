import sbt._
import Keys._
import _root_.scalafix.sbt.{BuildInfo => ScalafixBuildInfo}

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

lazy val root = (project in file("."))
  .aggregate(termflow, termflowSample)
  .settings(
    name          := "termflow",
    publish / skip := true
  )

lazy val termflow = (project in file("modules/termflow"))
  .settings(
    name := "termflow",
    description := "A small, functional terminal UI (TUI) framework for Scala",
    commonSettings,
    scalafixConfig := Some((ThisBuild / baseDirectory).value / ".scalafix-termflow.conf"),
    libraryDependencies ++= scalafixRuleDependencies.value,
    libraryDependencies ++= Seq(
      Deps.jline,
      Deps.pureconfigCore,
      Deps.pureconfigGenericScala3,
      Deps.scalatest % Test
    )
  )

lazy val termflowSample = (project in file("modules/termflow-sample"))
  .dependsOn(termflow % "compile->compile;test->test")
  .settings(
    name := "termflow-sample",
    commonSettings,
    libraryDependencies ++= Seq(
      Deps.jline,
      Deps.scalatest % Test
    ),
    coverageEnabled := false,
    publish / skip := true
  )

addCommandAlias("ciCheck", ";scalafmtCheckAll;scalafixAll --check;test")
addCommandAlias("coverageLib", ";project termflow;coverage;test;coverageReport")
addCommandAlias("prePR", ";ciCheck;coverageLib;termflowSample/runMain termflow.run.SampleSmoke")
addCommandAlias(
  "publishLocalFixed",
  s"""set ThisBuild / version := "$fixedLocalVersion"; publishLocal"""
)
