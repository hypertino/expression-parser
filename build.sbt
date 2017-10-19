crossScalaVersions := Seq("2.12.3", "2.11.11")

scalaVersion := crossScalaVersions.value.head

organization := "com.hypertino"

name := "expression-parser"

version := "0.2.1"

libraryDependencies ++= Seq(
  "com.hypertino"   %% "binders"        % "1.2.0",
  "org.parboiled"   %% "parboiled"      % "2.1.4",
  "org.scalamock"   %% "scalamock-scalatest-support" % "3.5.0" % "test"
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("public")
)

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
  </developers>

pgpSecretRing := file("./travis/script/ht-oss-private.asc")
pgpPublicRing := file("./travis/script/ht-oss-public.asc")
usePgpKeyHex("F8CDEF49B0EDEDCC")
pgpPassphrase := Option(System.getenv().get("oss_gpg_passphrase")).map(_.toCharArray)
publishMavenStyle := true
pomIncludeRepository := { _ => false}
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
credentials ++= (for {
  username <- Option(System.getenv().get("sonatype_username"))
  password <- Option(System.getenv().get("sonatype_password"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq

publishArtifact in Test := false
scalacOptions in Global ++= Seq("-feature", "-deprecation")
