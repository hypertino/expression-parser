organization := "eu.inn"

name := "expression-parser"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.lihaoyi"    %% "fastparse"  % "0.3.7",
  "eu.inn"         %% "binders-core" % "0.12.93",
  "org.parboiled"  %% "parboiled" % "2.1.3",
  "org.scalatest"  %% "scalatest"  % "2.2.6"     % "test"
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("public")
)

publishTo := Some("Innova libs repo" at "http://repproxy.srv.inn.ru/artifactory/libs-release-local")
credentials += Credentials(Path.userHome / ".ivy2" / ".innova_credentials")
