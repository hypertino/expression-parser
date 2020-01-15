import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val scala213 = "2.13.1"
lazy val scala212 = "2.12.10"
lazy val scala211 = "2.11.12"
lazy val supportedScalaVersions = List(scala213, scala212, scala211)

ThisBuild / scalaVersion := scala213

ThisBuild / organization := "com.hypertino"

ThisBuild / scalacOptions ++= Seq("-feature", "-deprecation")

lazy val expressionParser = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .settings(publishSettings:_*)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    name := "expression-parser",
    version := "0.3.0",
    libraryDependencies ++= Seq(
      "com.hypertino"   %%% "binders"        % "1.3.0",
      "org.parboiled"   %%% "parboiled"      % "2.1.8",
      "org.scalamock" %%% "scalamock" % "4.4.0" % Test,
      "org.scalatest" %% "scalatest" % "3.1.0" % Test
    ),
    publishArtifact := true,
    publishArtifact in Test := false,
    resolvers ++= Seq(
      Resolver.sonatypeRepo("public")
    )
  )
  .jsSettings(
  )
  .jvmSettings(
  )

lazy val js = expressionParser.js

lazy val jvm = expressionParser.jvm

lazy val `expressionParserRoot` = project.settings(publishSettings:_*).in(file("."))
  .settings(publishSettings:_*)
  .aggregate(js, jvm)
  .settings(
    crossScalaVersions := Nil,
    publish / skip := true
  )

val publishSettings = Seq(
  pomExtra := <url>https://github.com/hypertino/expression-parser</url>
    <licenses>
      <license>
        <name>BSD-style</name>
        <url>http://opensource.org/licenses/BSD-3-Clause</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:hypertino/expression-parser.git</url>
      <connection>scm:git:git@github.com:hypertino/expression-parser.git</connection>
    </scm>
    <developers>
      <developer>
        <id>maqdev</id>
        <name>Magomed Abdurakhmanov</name>
        <url>https://github.com/maqdev</url>
      </developer>
      <developer>
        <id>hypertino</id>
        <name>Hypertino</name>
        <url>https://github.com/hypertino</url>
      </developer>
    </developers>,
  publishMavenStyle := true,
  pomIncludeRepository := { _ => false},
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
)

Global / pgpPassphrase := Option(System.getenv().get("oss_gpg_passphrase")).map(_.toCharArray)
Global / credentials ++= Seq(
    Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", System.getenv().get("sonatype_username"), System.getenv().get("sonatype_password")),
  )
Global / useGpgAgent := false
usePgpKeyHex("97A4EB3D60277A26D5B5480BA53DC2FF4858319D")