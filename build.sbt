ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "scala-quill-delta",
    libraryDependencies ++= Seq(
      "io.spray" %% "spray-json" % "1.3.6",
      "org.scalatest" %% "scalatest" % "3.2.14" % Test
    ),
  )


