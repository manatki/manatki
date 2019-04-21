name := "manatki"

version := "0.1"

scalaVersion := "2.12.8"

val catsVersion = "1.6.0"
val catsEffectVersion = "1.2.0"
val catsMtlVersion = "0.4.0"
val monixVersion = "3.0.0-RC2"

val akkaVersion = "2.5.19"
val akkaHttpVersion = "10.1.3"

val fs2Version = "1.0.3"

val drosteVersion = "0.6.0"

val scalaTestVersion = "3.0.5"
val scalaCheckVersion = "1.13.4"

val akkas =
  (Seq("actor", "actor-typed", "stream").map(_ -> akkaVersion) :+ ("http" -> akkaHttpVersion))
    .map { case (module, libVersion) => "com.typesafe.akka" %% s"akka-$module" % libVersion }



libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
libraryDependencies += "org.typelevel" %% "cats-free" % catsVersion
libraryDependencies += "org.typelevel" %% "alleycats-core" % catsVersion
libraryDependencies += "org.typelevel" %% "cats-effect" % catsEffectVersion
libraryDependencies += "org.typelevel" %% "cats-mtl-core" % catsMtlVersion
libraryDependencies += "io.monix" %% "monix" % monixVersion
libraryDependencies += "io.higherkindness" %% "droste-core" % drosteVersion

scalacOptions ++= Seq(
  "-Ypartial-unification",

  "-deprecation",
  "-feature",

  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4")

val testSettings = Seq(
  libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion  % "test",
  libraryDependencies += "org.scalacheck" %% "scalacheck" % scalaCheckVersion  % "test"
)

lazy val akka = project.settings(libraryDependencies ++= akkas)

lazy val fsfs = project
  .settings(libraryDependencies += "co.fs2" %% "fs2-core" % fs2Version)
  .settings(testSettings)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies ++= akkas.map(_ % "test")

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.10" % "test"
libraryDependencies += "org.rudogma" %% "supertagged" % "1.4"