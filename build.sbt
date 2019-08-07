name := "manatki"

version in ThisBuild := "0.1"

scalaVersion in ThisBuild := "2.13.0"

val akkas =
  (List("actor", "actor-typed", "stream").map(_ -> Version.akka) :+ ("http" -> Version.akkaHttp)).map {
    case (module, libVersion) => "com.typesafe.akka" %% s"akka-$module" % libVersion
  }

libraryDependencies += "org.typelevel"              %% "cats-core"           % Version.cats
libraryDependencies += "org.typelevel"              %% "cats-free"           % Version.cats
libraryDependencies += "org.typelevel"              %% "cats-laws"           % Version.cats
libraryDependencies += "org.typelevel"              %% "alleycats-core"      % Version.cats
libraryDependencies += "org.typelevel"              %% "cats-effect"         % Version.catsEffect
libraryDependencies += "org.typelevel"              %% "cats-mtl-core"       % Version.catsMtl
libraryDependencies += "org.typelevel"              %% "cats-tagless-macros" % Version.catsTagless
//libraryDependencies += "io.higherkindness"          %% "droste-core"         % Version.droste
libraryDependencies += "ru.tinkoff"                 %% "tofu-core"           % Version.tofu
libraryDependencies += "ru.tinkoff"                 %% "tofu-concurrent"     % Version.tofu
libraryDependencies += "ru.tinkoff"                 %% "tofu-optics-core"    % Version.tofu
//libraryDependencies += "ru.tinkoff"                 %% "tofu-optics-interop" % Version.tofu
libraryDependencies += "com.github.julien-truffaut" %% "monocle-macro"       % Version.monocle
libraryDependencies += "com.github.julien-truffaut" %% "monocle-state"       % Version.monocle

//libraryDependencies += "io.monix"                   %% "monix"               % Version.monix
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
  addCompilerPlugin("org.typelevel"   %% "kind-projector"     % Version.kindProjector),
  addCompilerPlugin("com.olegpy"      %% "better-monadic-for" % Version.bm4)
)

val testSettings = List(
  libraryDependencies += "org.scalatest"  %% "scalatest"  % Version.scalaTest  % "test",
  libraryDependencies += "org.scalacheck" %% "scalacheck" % Version.scalaCheck % "test"
)

lazy val akka = project.settings(libraryDependencies ++= akkas)

lazy val fsfs = project
  .settings(libraryDependencies += "co.fs2" %% "fs2-core" % Version.fs2)
  .settings(testSettings)

lazy val problems = project
  .dependsOn(manatki)
  .settings(plugins)
  .settings(testSettings)

lazy val manatki = project.in(file(".")).settings(plugins)

testSettings

libraryDependencies ++= akkas.map(_ % "test")

libraryDependencies += "org.typelevel" %% "discipline-core" % Version.discipline
