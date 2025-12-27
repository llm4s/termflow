import sbt._
import Keys._

ThisBuild / organization := "org.llm4s"

val scala213 = Versions.scala213
val scala3   = Versions.scala3

ThisBuild / scalaVersion       := scala213
ThisBuild / crossScalaVersions := Seq(scala213, scala3)

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
