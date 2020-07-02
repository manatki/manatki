name := "manatki"

version in ThisBuild := "0.1"

scalaVersion in ThisBuild := "2.13.2"

val akkas =
  (List("actor", "actor-typed", "stream").map(_ -> Version.akka) :+ ("http" -> Version.akkaHttp)).map {
    case (module, libVersion) => "com.typesafe.akka" %% s"akka-$module" % libVersion
  }

val circes = List("core", "parser").map(module => "io.circe" %% s"circe-$module" % Version.circe)

libraryDependencies += "org.typelevel"              %% "cats-core"               % Version.cats withSources ()
libraryDependencies += "com.chuusai"                %% "shapeless"               % Version.shapeless withSources ()
libraryDependencies += "org.typelevel"              %% "cats-free"               % Version.cats
libraryDependencies += "org.typelevel"              %% "cats-laws"               % Version.cats
libraryDependencies += "org.typelevel"              %% "alleycats-core"          % Version.cats
libraryDependencies += "org.typelevel"              %% "cats-effect"             % Version.catsEffect
libraryDependencies += "org.typelevel"              %% "cats-mtl-core"           % Version.catsMtl
libraryDependencies += "org.typelevel"              %% "cats-tagless-macros"     % Version.catsTagless
libraryDependencies += "ru.tinkoff"                 %% "tofu-core"               % Version.tofu
libraryDependencies += "ru.tinkoff"                 %% "tofu-logging-derivation" % Version.tofu
libraryDependencies += "ru.tinkoff"                 %% "tofu-derivation"         % Version.tofu
libraryDependencies += "ru.tinkoff"                 %% "tofu-concurrent"         % Version.tofu
libraryDependencies += "ru.tinkoff"                 %% "tofu-optics-macro"       % Version.tofu
libraryDependencies += "com.github.julien-truffaut" %% "monocle-macro"           % Version.monocle
libraryDependencies += "com.github.julien-truffaut" %% "monocle-state"           % Version.monocle
libraryDependencies += "org.typelevel"              %% "simulacrum"              % Version.simulacrum
libraryDependencies += "org.typelevel"              %% "spire"                   % Version.spire

//libraryDependencies += "ru.tinkoff"                 %% "tofu-optics-interop" % Version.tofu
//libraryDependencies += "io.higherkindness"          %% "droste-core"         % Version.droste

libraryDependencies += "dev.zio" %% "zio"             % Version.zio
libraryDependencies += "dev.zio" %% "zio-macros-core" % Version.zioMacros
scalacOptions in ThisBuild ++= List(
  "-Ymacro-annotations",
  "-deprecation",
  "-feature",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps"
)

lazy val plugins = List(
  addCompilerPlugin("org.typelevel" %% "kind-projector"     % Version.kindProjector cross CrossVersion.patch),
  addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % Version.bm4)
)

val testSettings = List(
  libraryDependencies += "org.scalatest"     %% "scalatest"                % Version.scalaTest      % "test",
  libraryDependencies += "org.scalacheck"    %% "scalacheck"               % Version.scalaCheck     % "test",
  libraryDependencies += "org.scalatestplus" %% "scalatestplus-scalacheck" % Version.scalaTestCheck % "test",
)

lazy val akka = project.settings(libraryDependencies ++= akkas ++ circes)

lazy val fsfs = project
  .settings(
    libraryDependencies += "ru.tinkoff" %% "tofu-core"     % Version.tofu,
    libraryDependencies += ("co.fs2"    %% "fs2-io"        % Version.fs2) withSources (),
    libraryDependencies += "org.http4s" %% "http4s-server" % Version.http4s,
    plugins,
  )
  .settings(testSettings)

lazy val problems = project
  .dependsOn(manatki)
  .settings(plugins)
  .settings(testSettings)

lazy val manatki = project.in(file(".")).settings(plugins)

testSettings

libraryDependencies ++= akkas.map(_ % "test")

libraryDependencies += "org.typelevel" %% "discipline-core" % Version.discipline
