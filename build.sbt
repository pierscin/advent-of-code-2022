import Dependencies._

ThisBuild / organization := "pierscin"
ThisBuild / scalaVersion := "2.13.6"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code",
    libraryDependencies += Munit % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0"
  )
