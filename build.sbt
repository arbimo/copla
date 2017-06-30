name := "copla-build"

// global settings
val _organization = "fr.bitmonnot.copla"
val _version = "0.1-SNAPSHOT"
val _scalaVersion = "2.12.2"

lazy val root = project.in(file(".")).
  aggregate(coplaLang, coplaConstraints).
  settings(
    publish := {},
    publishLocal := {}
  )

version := _version
scalaVersion := _scalaVersion
organization := _organization
crossPaths := true

initialize := {
  val _ = initialize.value
  if (sys.props("java.specification.version") != "1.8")
    sys.error("Java 8 is required for this project.")
}

lazy val commonSettings = Seq(
  organization := _organization,
  version := _version,
  crossPaths := true,
  exportJars := true, // insert other project dependencies in oneJar
  scalaVersion := _scalaVersion,
  javaOptions in run ++= Seq("-Xmx1000m", "-ea")
)

lazy val coplaLang = project.in(file("copla-lang"))
  .settings(name := "copla-lang")
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "com.lihaoyi" %% "fastparse" % "0.4.3",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  ))

lazy val coplaConstraints = project.in(file("copla-constraints"))
  .settings(name := "copla-constraints")
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  ))
