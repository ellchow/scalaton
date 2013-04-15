import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "scalaton"
  val buildVersion      = "0.1-SNAPSHOT"
  val buildScalaVersion = "2.10.1"

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
  val nictaAvro = "nicta's avro" at "http://nicta.github.com/scoobi/releases"
  val cloudera = "cloudera" at "https://repository.cloudera.com/content/repositories/releases/"
  val localm2 = "local m2 repo" at "file://" + Path.userHome.absolutePath + "/.m2/repository"

  val allResolvers = Seq(sonatypeReleases, sonatypeSnapshots, cloudera, scalaTools, typesafe, nictaAvro, localm2)
}

object Dependencies {
  val scalaz7 = "org.scalaz" %% "scalaz-core" % "7.0.0-M9"
  val scoobi = "com.nicta" %% "scoobi" % "0.7.0-cdh3-SNAPSHOT"
  val spire = "org.spire-math" %% "spire" % "0.3.0"
  val breezemath = "org.scalanlp" %% "breeze-math" % "0.3-SNAPSHOT"
  val scalaz7effect = "org.scalaz" %% "scalaz-effect" % "7.0.0-M8"
  val javaewah = "com.googlecode.javaewah" % "JavaEWAH" % "0.6.6"
  val opencsv = "net.sf.opencsv" % "opencsv" % "2.3"
  val scalatime = "com.github.nscala-time" %% "nscala-time" % "0.2.0"
  val specs2 = "org.specs2" %% "specs2" % "1.14" % "test"
  val apacheCommonsIo = "org.apache.commons" % "commons-io" % "1.3.2"
  val clHashMap = "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.3.2"
}

object ProjectBuild extends Build{
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  val utilDeps = Seq(
    scalaz7, specs2,
    apacheCommonsIo, opencsv, clHashMap, scalatime
  )

  val aggregateDeps = Seq(
    scalaz7, specs2,
    javaewah, breezemath
  )

  val zedDeps = Seq(
    scalaz7, specs2,
    scalaz7effect, scalatime
  )

  val dooDeps = Seq(
    scalaz7, specs2,
    scoobi
  )

  val compilerOptions = Seq(
    "-deprecation",
    "-feature",
    "-language:postfixOps",
    "-language:higherKinds",
    "-language:implicitConversions"
  )

  val publishLoc = Some(Resolver.file("local m2", new File( Path.userHome.absolutePath + "/.m2/repository" )))

  lazy val utilProject = Project (
    "util",
    file ("scalaton-util"),
    settings = buildSettings ++ Seq(
      resolvers := allResolvers,
      libraryDependencies ++= utilDeps,
      scalacOptions := compilerOptions,
      publishTo := publishLoc)
  )

  lazy val zedProject = Project (
    "zed",
    file ("scalaton-zed"),
    settings = buildSettings ++ Seq(
      resolvers := allResolvers,
      libraryDependencies ++= zedDeps,
      scalacOptions := compilerOptions,
      publishTo := publishLoc)
  )


  lazy val aggregateProject = Project (
    "aggregate",
    file ("scalaton-aggregate"),
    settings = buildSettings ++ Seq(
      resolvers := allResolvers,
      libraryDependencies ++= aggregateDeps,
      scalacOptions := compilerOptions,
      publishTo := publishLoc)
  ) dependsOn (utilProject, zedProject)

  lazy val dooProject = Project (
    "doo",
    file ("scalaton-doo"),
    settings = buildSettings ++ Seq(
      resolvers := allResolvers,
      libraryDependencies ++= dooDeps,
      scalacOptions := compilerOptions,
      publishTo := publishLoc
    )
  ) dependsOn (utilProject, aggregateProject, zedProject)


  lazy val root = Project(
    "everything",
    file("."),
    settings = buildSettings ++ Seq(
      scalacOptions := compilerOptions,
      publishTo := publishLoc)
  ) aggregate(utilProject, zedProject, aggregateProject, dooProject)

}






