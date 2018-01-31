name := "manatki"

version := "0.1"

scalaOrganization := "org.typelevel"
scalaVersion := "2.12.4-bin-typelevel-4"

val catsVersion = "1.0.1"
val catsEffectVersion = "0.5"
val catsMtlVersion = "0.2.1"

libraryDependencies += "org.typelevel" %% "cats-core"     % catsVersion
libraryDependencies += "org.typelevel" %% "cats-free"     % catsVersion
libraryDependencies += "org.typelevel" %% "cats-effect"   % catsEffectVersion
libraryDependencies += "org.typelevel" %% "cats-mtl-core" % catsMtlVersion

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