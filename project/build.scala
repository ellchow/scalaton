import sbt._
import Keys._

import sbtassembly.Plugin._
import AssemblyKeys._

import spray.revolver.RevolverPlugin.Revolver


object ProjectBuild extends Build{
  /** Settings **/
  val Organization = "scalaton"
  val Version      = "0.1-SNAPSHOT"
  val ScalaVersion = "2.10.2"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := Organization,
    version      := Version,
    scalaVersion := ScalaVersion,
    shellPrompt  := ShellPrompt.buildShellPrompt,
    resolvers := Dependencies.resolvers
  )

  val customAssemblySettings = Seq(
    mergeStrategy in assembly <<= (mergeStrategy in assembly) { old => {
      case x =>
        val oldstrat = old(x)
        if (oldstrat == MergeStrategy.deduplicate) MergeStrategy.first else oldstrat
    }}
  )

  val compilerOptions = Seq(
    // "-Xlog-implicits",
    "-deprecation",
    "-feature",
    "-language:_",
    "-encoding",
    "utf8"
  )

  /** Shell Prompt **/
  object ShellPrompt {
    object devnull extends ProcessLogger {
      def info (s: => String) {}
      def error (s: => String) { }
      def buffer[T] (f: => T): T = f
    }
    def currBranch = (
      ("git status -sb" lines_! devnull headOption)
      getOrElse "-" stripPrefix "## "
    )

    val buildShellPrompt = {
      (state: State) => {
        val currProject = Project.extract (state).currentProject.id
        "%s:%s:%s> ".format (
          currProject, currBranch, Version
        )
      }
    }
  }

  /** Projects **/

  val getPublishLoc = { (v: String) =>
    if (v.trim.endsWith("SNAPSHOT"))
      Some(Resolver.file("ellchow mvn snaphsots", new File( "./publish-repo/snapshots" )))
    else
      Some(Resolver.file("ellchow mvn releases", new File( "./publish-repo/releases" )))
  }

  lazy val utilProject = Project (
    "util",
    file ("util"),
    settings = buildSettings ++ assemblySettings ++ customAssemblySettings ++ Seq(
      libraryDependencies ++= Dependencies.util,
      scalacOptions := compilerOptions,
      publishTo <<= version(getPublishLoc)
    )
  )

  lazy val aggregateProject = Project (
    "aggregate",
    file ("aggregate"),
    settings = buildSettings ++ assemblySettings ++ customAssemblySettings ++ Seq(
      libraryDependencies ++= Dependencies.aggregate,
      scalacOptions := compilerOptions,
      publishTo <<= version(getPublishLoc)
    )
  ) dependsOn(utilProject)

  lazy val dooProject = Project (
    "doo",
    file ("doo"),
    settings = buildSettings ++ assemblySettings ++ customAssemblySettings ++ Seq(
      libraryDependencies ++= Dependencies.doo,
      scalacOptions := compilerOptions,
      publishTo <<= version(getPublishLoc)
    )
  ) dependsOn(utilProject, aggregateProject)

}






