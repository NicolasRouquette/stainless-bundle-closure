ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.12.9"

lazy val core = project
  .in(file("core"))
  .settings(
    name := "bundle-closure",
    libraryDependencies += "net.sourceforge.owlapi" % "owlapi-distribution" % "5.1.15",
    libraryDependencies += "org.jgrapht" % "jgrapht-core" % "1.1.0",
  )
  .dependsOn(verified)

lazy val verified = project
  .in(file("verified"))
  .enablePlugins(StainlessPlugin)
  .settings(
    name := "bundle-closure-verified",
    stainlessEnabled := true,
  )
