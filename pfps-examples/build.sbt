import Dependencies._
val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "pfps-examples",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      Libraries.cats,
      Libraries.catsEffect,
      Libraries.kittens,
      Libraries.circeCore,
      Libraries.circeParser,
      Libraries.circeExtras,
      Libraries.circeRefined,
      Libraries.monocleCore,
      Libraries.munit,
    )
  )
