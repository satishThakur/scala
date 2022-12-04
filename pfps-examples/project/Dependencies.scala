import sbt._

object Dependencies {
  object V {
    val cats       = "2.7.0"
    val catsEffect = "3.3.12"
    val fs2        = "3.2.8"
    val monocle    = "3.1.0"
    val kittens    = "3.0.0-M1"
    val circe      = "0.14.1"
    val munit      = "0.7.29"
  }
  object Libraries {
    val cats         = "org.typelevel" %% "cats-core"     % V.cats
    val catsEffect   = "org.typelevel" %% "cats-effect"   % V.catsEffect
    val fs2          = "co.fs2"        %% "fs2-core"      % V.fs2

    val kittens      = "org.typelevel" %% "kittens"       % V.kittens

    val circeCore    = "io.circe"      %% "circe-core"    % V.circe
    val circeParser  = "io.circe"      %% "circe-parser"  % V.circe
    val circeExtras  = "io.circe"      %% "circe-extras"  % V.circe
    val circeRefined = "io.circe"      %% "circe-refined" % V.circe

    val monocleCore  = "dev.optics"    %% "monocle-core"  % V.monocle
    val monocleMacro = "dev.optics"    %% "monocle-macro" % V.monocle
    val munit        = "org.scalameta" %% "munit"         % V.munit        % Test

  }
}

