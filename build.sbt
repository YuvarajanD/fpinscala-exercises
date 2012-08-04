/** Definition */
name := "fpinscala-exercises"

organization := "etorreborre"

version := "1.0"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.12" % "test",
  "org.scalacheck" %% "scalacheck" % "1.9" % "test",
  "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT",
)

/** Testing */
testOptions := Seq(Tests.Filter(s => s.endsWith("Spec"))