import sbt._
import Keys._

val scala3   = Versions.scala3

inThisBuild(
  List(
    organization       := "org.llm4s",
    organizationName   := "llm4s",
    scalaVersion       := scala3,
    crossScalaVersions := Seq(scala3),
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
    publishTo := {
      val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
      val centralReleases  = "https://central.sonatype.com/repository/maven-releases/"
      if (isSnapshot.value) Some("central-snapshots".at(centralSnapshots))
      else Some("central-releases".at(centralReleases))
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
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature"
  ),
  Test / fork := true
)

lazy val root = (project in file("."))
  .aggregate(termflow, termflowSample)
  .settings(
    name          := "termflow",
    publish / skip := true
  )

lazy val termflow = (project in file("modules/termflow"))
  .settings(
    name := "termflow",
    commonSettings,
    libraryDependencies ++= Seq(
      Deps.jline,
      Deps.scalatest % Test
    )
  )

lazy val termflowSample = (project in file("modules/termflow-sample"))
  .dependsOn(termflow)
  .settings(
    name := "termflow-sample",
    commonSettings,
    libraryDependencies ++= Seq(
      Deps.jline
    ),
    publish / skip := true
  )
