
ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "floor2024",
    Compile / unmanagedSourceDirectories += baseDirectory.value / "src"
  )
