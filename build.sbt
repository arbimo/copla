name := "copla-build"

// global settings
lazy val commonSettings = Seq(
  organization := "com.github.arthur-bit-monnot",
  version := "0.1-SNAPSHOT",
  crossPaths := true
)

lazy val commonJVMSettings = Seq(
  scalaVersion := "2.12.3",
  exportJars := true, // insert other project dependencies in oneJar
  javaOptions in run ++= Seq("-Xmx1000m", "-ea"),
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.1" % "test"
)

lazy val commonNativeSettings = Seq(
  scalaVersion := "2.11.11"
)

lazy val root = project.in(file(".")).
  aggregate(coplaLangJVM, coplaConstraints).
  settings(
    publish := {},
    publishLocal := {}
  )


lazy val coplaLang = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure) // keep usual repository organization
  .in(file("copla-lang"))
  .settings(name := "copla-lang")
  .settings(commonSettings: _*)
  .jvmSettings(commonJVMSettings: _*)
  .nativeSettings(commonNativeSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "fastparse" % "0.4.4",
    "com.github.scopt" %%% "scopt" % "3.7.0"
  ))

lazy val coplaLangJVM = coplaLang.jvm
lazy val coplaLangNative = coplaLang.native

lazy val coplaConstraints = project.in(file("copla-constraints"))
  .settings(name := "copla-constraints")
  .settings(commonSettings: _*)
  .settings(commonJVMSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  ))
