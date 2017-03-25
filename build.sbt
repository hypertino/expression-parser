crossScalaVersions := Seq("2.12.1", "2.11.8")

scalaVersion in Global := "2.12.1"


organization := "com.hypertino"

name := "expression-parser"

version := "0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.hypertino"   %% "binders"        % "1.0-SNAPSHOT",
  "org.parboiled"   %% "parboiled"      % "2.1.4",
  "org.scalamock"   %% "scalamock-scalatest-support" % "3.5.0" % "test"
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("public")
)
