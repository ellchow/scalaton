import sbt._
import Keys._

import sbtassembly.Plugin._
import AssemblyKeys._

import spray.revolver.RevolverPlugin.Revolver


object ProjectBuild extends Build{
  /** Settings **/
  val Organization = "com.github.ellchow"
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

  val publishLoc = Some(Resolver.file("local m2", new File( Path.userHome.absolutePath + "/.m2/repository" )))

  lazy val utilProject = Project (
    "util",
    file ("util"),
    settings = buildSettings ++ assemblySettings ++ customAssemblySettings ++ Seq(
      libraryDependencies ++= Dependencies.util,
      scalacOptions := compilerOptions,
      publishTo := publishLoc
    )
  )

  lazy val aggregateProject = Project (
    "aggregate",
    file ("aggregate"),
    settings = buildSettings ++ assemblySettings ++ customAssemblySettings ++ Seq(
      libraryDependencies ++= Dependencies.aggregate,
      scalacOptions := compilerOptions,
      publishTo := publishLoc
    )
  ) dependsOn(utilProject)

  lazy val dooProject = Project (
    "doo",
    file ("doo"),
    settings = buildSettings ++ assemblySettings ++ customAssemblySettings ++ Seq(
      libraryDependencies ++= Dependencies.doo,
      scalacOptions := compilerOptions,
      publishTo := publishLoc
    )
  ) dependsOn(utilProject, aggregateProject)

  lazy val root = Project(
    "root",
    file("."),
    settings = buildSettings ++ assemblySettings ++ customAssemblySettings ++ Seq(
      scalacOptions := compilerOptions,
      publishTo := publishLoc
    )
  ) aggregate(utilProject, aggregateProject, dooProject)


}






