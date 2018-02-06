name := "manatki"

version := "0.1"

scalaOrganization := "org.typelevel"
scalaVersion := "2.12.4-bin-typelevel-4"

val catsVersion = "1.0.1"
val catsEffectVersion = "0.5"
val catsMtlVersion = "0.2.1"
val monixVersion = "3.0.0-M3"

val akkaVersion = "2.5.9"
val akkaHttpVersion = "10.0.11"

val akkas =
  (Seq("actor", "actor-typed", "stream").map(_ -> akkaVersion) :+ ("http" -> akkaHttpVersion))
    .map { case (module, libVersion) => "com.typesafe.akka" %% s"akka-$module" % libVersion }

libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
libraryDependencies += "org.typelevel" %% "cats-free" % catsVersion
libraryDependencies += "org.typelevel" %% "cats-effect" % catsEffectVersion
libraryDependencies += "org.typelevel" %% "cats-mtl-core" % catsMtlVersion
libraryDependencies += "io.monix" %% "monix" % monixVersion

scalacOptions ++= Seq(
  "-Ypartial-unification",
  "-Yliteral-types",
  "-Yinduction-heuristics",

  "-Xstrict-patmat-analysis",

  "-deprecation",
  "-feature",

  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M11" cross CrossVersion.patch)

lazy val akka = project.settings(libraryDependencies ++= akkas)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies ++= akkas.map(_ % "test")