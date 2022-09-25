ThisBuild / scalaVersion     := "2.13.6"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "ru.bkhabib"

lazy val root = (project in file("."))
  .settings(
    name := "bhvalid"
  )