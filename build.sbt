ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "scala2-sbt-template",
    // Common dependencies
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
    // Command Line Parser - https://github.com/scopt/scopt
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1" ,
    // Testing dependencies
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test",
    libraryDependencies += "org.scalatest" %% "scalatest-funsuite" % "3.2.11" % "test"
  )
