import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "scalaton"
  val buildVersion      = "0.1-SNAPSHOT"
  val buildScalaVersion = "2.9.2"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion,
    shellPrompt  := ShellPrompt.buildShellPrompt
  )
}

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
        currProject, currBranch, BuildSettings.buildVersion
      )
    }
  }
}

object Resolvers {
  val sonatypeSnapshots = "sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
  val sonatypeReleases = "sonatype releases"  at "http://oss.sonatype.org/content/repositories/releases"
  val scalaTools = "scala tools" at "http://scala-tools.org/repo-releases"
  val typesafe = "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
  val localm2 = "local m2 repo" at "file://" + Path.userHome.absolutePath + "/.m2/repository"

  val allResolvers = Seq(sonatypeReleases, sonatypeSnapshots, scalaTools, typesafe, localm2)
}

object Dependencies {
  val scalaz7 = "org.scalaz" %% "scalaz-core" % "7.0.0-M3"
  val javaewah = "com.googlecode.javaewah" % "JavaEWAH" % "0.6.6"
  val opencsv = "net.sf.opencsv" % "opencsv" % "2.3"
  val scalatime = "com.github.nscala-time" %% "nscala-time" % "0.2.0"
  val specs2 = "org.specs2" %% "specs2" % "1.12.3" % "test"
  val apacheCommonsIo = "org.apache.commons" % "commons-io" % "1.3.2"
}

object ProjectBuild extends Build{
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  val commonDeps = Seq(scalaz7, specs2)

  val utilDeps = Seq(apacheCommonsIo, opencsv, scalatime)

  val aggregateDeps = Seq(javaewah)

  val compilerOptions = Seq(
    "-deprecation",
    "-Ydependent-method-types",
    "-unchecked"
  )

  val publishLoc = Some(Resolver.file("local m2", new File( Path.userHome.absolutePath + "/.m2/repository" )))

  lazy val utilProject = Project (
    "util",
    file ("scalaton-util"),
    settings = buildSettings ++ Seq(
      resolvers := allResolvers,
      libraryDependencies ++= commonDeps ++ utilDeps,
      scalacOptions := compilerOptions,
      publishTo := publishLoc)
  )

  lazy val aggregateProject = Project (
    "aggregate",
    file ("scalaton-aggregate"),
    settings = buildSettings ++ Seq(
      resolvers := allResolvers,
      libraryDependencies ++= commonDeps ++ aggregateDeps,
      scalacOptions := compilerOptions,
      publishTo := publishLoc)

  ) dependsOn (utilProject)

}





