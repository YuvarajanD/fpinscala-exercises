/** Definition */
name := "fpinscala-exercises"

organization := "etorreborre"

version := "1.0"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scalaz"         %% "scalaz-core"    % "7.0.4",
  "org.scalaz.stream"  %% "scalaz-stream"    % "0.2",
  "org.specs2"         %% "specs2-core"     % "2.3.4"   % "test",
  "org.specs2"         %% "specs2-scalacheck"     % "2.3.4"   % "test",
  "org.specs2"         %% "specs2-mock"     % "2.3.4"   % "test",
  "org.hamcrest"       %  "hamcrest-core"     % "1.3"   % "test",
  "org.mockito"        %  "mockito-core"     % "1.9.5"   % "test",
  "org.objenesis"      %  "objenesis"     % "1.2"   % "test",
  "org.scalacheck"     %% "scalacheck"  % "1.11.1"  % "test"
)

scalacOptions ++= Seq("-deprecation", "-Yrangepos", "-unchecked", "-Ywarn-adapted-args")

resolvers ++= Seq(Resolver.sonatypeRepo("snaspshots"),    Resolver.sonatypeRepo("releases"))