organization := "eu.inn"

name := "expression-parser"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.lihaoyi"    %% "fastparse"  % "0.3.7",
  "org.scalatest"  %% "scalatest"  % "2.2.6"     % "test"
)
