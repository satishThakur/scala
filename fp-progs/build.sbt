val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "fp-progs",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "com.lihaoyi" %% "requests" % "0.7.0",

    libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.10",

    libraryDependencies += "com.lihaoyi" %% "upickle" % "1.5.0",

    libraryDependencies += ("com.storm-enroute" %% "scalameter-core" % "0.19").cross(CrossVersion.for3Use2_13)
  )


/**
name := "fp-progs"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies += "com.lihaoyi" %% "requests" % "0.5.1"

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.6"

libraryDependencies += "com.lihaoyi" %% "upickle" % "0.9.5"

libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.19",
**/