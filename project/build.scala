import sbt._
import Keys._

object ProjectBuild extends Build{
  /** Settings **/
  val Organization = "com.github.ellchow"
  val Version      = "0.1.2-SNAPSHOT"
  val ScalaVersion = "2.10.3"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := Organization,
    version      := Version,
    scalaVersion := ScalaVersion,
    shellPrompt  := ShellPrompt.buildShellPrompt,
    resolvers := Dependencies.resolvers
  )

  val publishSettings = Seq(
    publishMavenStyle := true,

    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },

    pomIncludeRepository := { _ => false },

    pomExtra := (
      <url>https://github.com/ellchow/scalaton</url>
      <licenses>
        <license>
          <name>Apache 2.0</name>
          <url>http://www.opensource.org/licenses/Apache-2.0</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
      <url>git@github.com:ellchow/scalaton.git</url>
      <connection>scm:git:git@github.com:ellchow/scalaton.git</connection>
      </scm>
      <developers>
        <developer>
          <id>ellchow</id>
          <name>Elliot Chow</name>
          <url>http://github.com/ellchow</url>
        </developer>
      </developers>
    )
  )

  val compilerOptions = Seq(
    // "-Xlog-implicits",
    "-deprecation",
    "-feature",
    "-language:_",
    "-encoding",
    "utf8",
    "-target:jvm-1.6"
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

  // val publishLoc = Some(Resolver.file("local m2", new File( Path.userHome.absolutePath + "/.m2/repository" )))

  lazy val utilProject = Project (
    "scalaton-util",
    file ("util"),
    settings = buildSettings ++ publishSettings ++ Seq(
      libraryDependencies ++= Dependencies.util,
      scalacOptions := compilerOptions
    )
  )

  lazy val streamProject = Project (
    "scalaton-stream",
    file ("stream"),
    settings = buildSettings ++ publishSettings ++ Seq(
      libraryDependencies ++= Dependencies.stream,
      scalacOptions := compilerOptions
    )
  ) dependsOn(utilProject)

  lazy val collectionProject = Project (
    "scalaton-collection",
    file ("collection"),
    settings = buildSettings ++ publishSettings ++ Seq(
      libraryDependencies ++= Dependencies.collection,
      scalacOptions := compilerOptions
    )
  ) dependsOn(utilProject)

  lazy val aggregateProject = Project (
    "scalaton-aggregate",
    file ("aggregate"),
    settings = buildSettings ++ publishSettings ++ Seq(
      libraryDependencies ++= Dependencies.aggregate,
      scalacOptions := compilerOptions
    )
  ) dependsOn(utilProject, collectionProject)

  lazy val akkaProject = Project (
    "scalaton-akka",
    file ("akka"),
    settings = buildSettings ++ publishSettings ++ Seq(
      libraryDependencies ++= Dependencies.akka,
      scalacOptions := compilerOptions
    )
  ) dependsOn(collectionProject)


  lazy val root = Project(
    "scalaton",
    file("."),
    settings = buildSettings ++ publishSettings ++ Seq(
      scalacOptions := compilerOptions,
      publishArtifact := false
    )
  ) aggregate(utilProject, aggregateProject, collectionProject, akkaProject)


}
