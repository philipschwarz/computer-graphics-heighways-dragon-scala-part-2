ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.5"

lazy val root = (project in file("."))
  .settings(
    name := "computer-graphics-heighways-dragon-scala-part-2",
    libraryDependencies += "io.github.dieproht" %% "matr-bundle" % "0.0.3"
  )
