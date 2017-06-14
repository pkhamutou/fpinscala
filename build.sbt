import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.pkhamutou",
      scalaVersion := "2.11.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Fpinscala",
    libraryDependencies ++= Seq(
      scalaTest % Test,
     "org.scalacheck" %% "scalacheck" % "1.13.5" % Test
    )
  )
