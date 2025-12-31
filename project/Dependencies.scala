import sbt._

object Versions {
  val scala213 = "2.13.16"
  val scala3   = "3.7.1"

  val jline     = "3.30.6"
  val scalatest = "3.2.19"
}

object Deps {
  val jline     = "org.jline" % "jline" % Versions.jline
  val scalatest = "org.scalatest" %% "scalatest" % Versions.scalatest
}
