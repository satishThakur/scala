import sbt.Keys.libraryDependencies

val scala3Version = "3.1.0"
val catVersion = "2.7.0"
val catEffectVersion = "3.3.12"
val munitVersion = "0.7.29"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala-cat-core",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.typelevel" %% "cats-core" % catVersion,
    libraryDependencies += "org.scalameta" %% "munit" % munitVersion % "test",
    libraryDependencies += "org.typelevel" %% "cats-effect" % catEffectVersion
)
