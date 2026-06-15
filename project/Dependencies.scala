import sbt._

object Versions {
  val scala3   = "3.7.1"

  val jline     = "3.30.6"
  val pureconfig = "0.17.10"
  val scalatest = "3.2.19"
  // ScalaCheck and the matching scalatest+scalacheck bridge. The bridge
  // version tracks the scalatest version (3.2.19.x) and targets scalacheck 1.18.
  val scalacheck     = "1.18.1"
  val scalatestPlusSc = "3.2.19.0"
}

object Deps {
  val jline                  = "org.jline" % "jline" % Versions.jline
  val pureconfigCore         = "com.github.pureconfig" %% "pureconfig-core" % Versions.pureconfig
  val pureconfigGenericScala3 =
    "com.github.pureconfig" %% "pureconfig-generic-scala3" % Versions.pureconfig
  val scalatest             = "org.scalatest" %% "scalatest" % Versions.scalatest
  val scalacheck            = "org.scalacheck" %% "scalacheck" % Versions.scalacheck
  val scalatestPlusScalacheck =
    "org.scalatestplus" %% "scalacheck-1-18" % Versions.scalatestPlusSc
}
