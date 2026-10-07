ThisBuild / scalaVersion := "3.3.8"
ThisBuild / organization := "org.tptp"
ThisBuild / version := "0.0.1"
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature")

val antlrVersion = "4.13.2"
val munit = "org.scalameta" %% "munit" % "1.3.6" % Test
val munitScalacheck = "org.scalameta" %% "munit-scalacheck" % "1.3.1" % Test

lazy val root = (project in file("."))
  .aggregate(core, server)
  .settings(publish / skip := true)

lazy val core = project
  .enablePlugins(Antlr4Plugin)
  .settings(
    name := "tptp-lsp-core",
    Antlr4 / antlr4Version := antlrVersion,
    Antlr4 / antlr4PackageName := Some("tptp.syntax.generated"),
    Antlr4 / antlr4GenListener := false,
    Antlr4 / antlr4GenVisitor := false,
    libraryDependencies ++= Seq("org.antlr" % "antlr4-runtime" % antlrVersion, munit, munitScalacheck),
    // Forked tests run with core/ as working directory, so golden files resolve as src/test/resources/...
    Test / fork := true,
    // the full mutation run (tptp.mutation.MutationRun) runs forked with room for large axiom files
    Test / run / fork := true,
    Test / run / javaOptions ++= Seq("-Xmx5g", "-Xss64m"),
  )

lazy val server = project
  .dependsOn(core)
  .settings(
    name := "tptp-lsp-server",
    libraryDependencies ++= Seq("org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "1.0.0", munit),
    assembly / mainClass := Some("tptp.server.Main"),
    assembly / assemblyJarName := "tptp-lsp.jar",
    assembly / assemblyMergeStrategy := {
      case "module-info.class"                                      => MergeStrategy.discard
      case PathList("META-INF", "versions", _, "module-info.class") => MergeStrategy.discard
      case other =>
        val previous = (assembly / assemblyMergeStrategy).value
        previous(other)
    },
  )
