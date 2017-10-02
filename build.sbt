name := "copla-build"

// global settings
lazy val commonSettings = Seq(
  organization := "com.github.arthur-bit-monnot",
  version := "0.2-SNAPSHOT",
  crossPaths := true,
  
  // To sync with Maven central
  publishMavenStyle := true,

  // POM settings for Sonatype
  homepage := Some(url("https://github.com/arthur-bit-monnot/copla")),
  scmInfo := Some(ScmInfo(url("https://github.com/arthur-bit-monnot/copla"), "git@github.com:arthur-bit-monnot/fape.git")),
  developers += Developer("abitmonn", "Arthur Bit-Monnot", "arthur.bit-monnot@laas.fr", url("https://github.com/arthur-bit-monnot")),
  licenses += ("BSD-2-Clause", url("https://opensource.org/licenses/BSD-2-Clause")),
  pomIncludeRepository := (_ => false)
)

lazy val commonJVMSettings = Seq(
  scalaVersion := "2.12.3",
  exportJars := true, // insert other project dependencies in oneJar
  javaOptions in run ++= Seq("-Xmx1000m", "-ea"),
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

lazy val root = project.in(file(".")).
  aggregate(coplaLang, coplaConstraints, coplaPlanning).
  settings(
    publish := {},
    publishLocal := {}
  )


lazy val coplaLang = project
  .in(file("copla-lang"))
  .settings(name := "copla-lang")
  .settings(commonSettings: _*)
  .settings(commonJVMSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "com.lihaoyi" %% "fastparse" % "0.4.4",
    "com.github.scopt" %% "scopt" % "3.7.0",
    "com.github.arthur-bit-monnot" %% "landscaper" % "0.1.1"
  ))

lazy val coplaConstraints = project.in(file("copla-constraints"))
  .settings(name := "copla-constraints")
  .settings(commonSettings: _*)
  .settings(commonJVMSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.2",
    "com.lihaoyi" %% "sourcecode" % "0.1.4",
    "org.typelevel" %% "cats-core" % "1.0.0-MF",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "biz.enef" %% "slogging" % "0.5.3",
    "biz.enef" %% "slogging-slf4j" % "0.5.3",
    "org.slf4j" % "slf4j-simple" % "1.7.12" % "optional"
  ))

lazy val coplaPlanning = project.in(file("copla-planning"))
  .dependsOn(coplaConstraints, coplaLang)
  .settings(name := "copla-planning")
  .settings(commonSettings: _*)
  .settings(commonJVMSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "com.github.scopt" %% "scopt" % "3.7.0",
    "biz.enef" %% "slogging" % "0.5.3",
    "org.slf4j" % "slf4j-simple" % "1.7.12"
  ))

lazy val coplaExp = project.in(file("copla-exp"))
  .dependsOn(coplaPlanning)
  .settings(name := "copla-exp")
  .settings(commonSettings: _*)
  .settings(commonJVMSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.slf4j" % "slf4j-simple" % "1.7.12"
  ))