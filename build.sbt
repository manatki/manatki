name := "manatki"

version := "0.1"

scalaVersion := "2.12.8"

val catsVersion        = "2.0.0-M4"
val catsEffectVersion  = "1.2.0"
val catsMtlVersion     = "0.4.0"
val catsTaglessVersion = "0.5"
val monixVersion       = "3.0.0-RC2"
val monocleVersion     = "1.5.1-cats"

val akkaVersion     = "2.5.19"
val akkaHttpVersion = "10.1.3"

val fs2Version = "1.0.4"

val drosteVersion = "0.6.0"

val scalaTestVersion  = "3.0.5"
val scalaCheckVersion = "1.13.4"
val tofuVersion       = "0.1"

val akkas =
  (Seq("actor", "actor-typed", "stream").map(_ -> akkaVersion) :+ ("http" -> akkaHttpVersion)).map {
    case (module, libVersion) => "com.typesafe.akka" %% s"akka-$module" % libVersion
  }

libraryDependencies += "org.typelevel"              %% "cats-core"           % catsVersion
libraryDependencies += "org.typelevel"              %% "cats-free"           % catsVersion
libraryDependencies += "org.typelevel"              %% "cats-laws"           % catsVersion
libraryDependencies += "org.typelevel"              %% "alleycats-core"      % catsVersion
libraryDependencies += "org.typelevel"              %% "cats-effect"         % catsEffectVersion
libraryDependencies += "org.typelevel"              %% "cats-mtl-core"       % catsMtlVersion
libraryDependencies += "org.typelevel"              %% "cats-tagless-macros" % catsTaglessVersion
libraryDependencies += "io.monix"                   %% "monix"               % monixVersion
libraryDependencies += "io.higherkindness"          %% "droste-core"         % drosteVersion
libraryDependencies += "ru.tinkoff"                 %% "tofu-core"           % tofuVersion
libraryDependencies += "com.github.julien-truffaut" %% "monocle-macro"       % monocleVersion
libraryDependencies += "com.github.julien-truffaut" %% "monocle-state"       % monocleVersion

scalacOptions in ThisBuild ++= Seq(
  "-Ypartial-unification",
  "-deprecation",
  "-feature",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps"
)

lazy val plugins = List(
  addCompilerPlugin("org.typelevel"   %% "kind-projector"     % "0.10.3"),
  addCompilerPlugin("org.scalamacros" % "paradise"            % "2.1.1" cross CrossVersion.patch),
  addCompilerPlugin("com.olegpy"      %% "better-monadic-for" % "0.3.0-M4")
)

val testSettings = Seq(
  libraryDependencies += "org.scalatest"  %% "scalatest"  % scalaTestVersion  % "test",
  libraryDependencies += "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test"
)

lazy val akka = project.settings(libraryDependencies ++= akkas)

lazy val fsfs = project
  .settings(libraryDependencies += "co.fs2" %% "fs2-core" % fs2Version)
  .settings(testSettings)

lazy val problems = project
  .dependsOn(manatki)
  .settings(plugins)
  .settings(testSettings)

lazy val manatki = project.in(file(".")).settings(plugins)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies ++= akkas.map(_ % "test")

libraryDependencies += "com.storm-enroute" %% "scalameter"  % "0.10" % "test"
libraryDependencies += "org.rudogma"       %% "supertagged" % "1.4"

libraryDependencies += "org.typelevel" %% "discipline" % "0.10.0"
