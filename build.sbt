import AssemblyKeys._

organization := "scalaton"

name := "scalaton"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.0"

checksums in update := Nil

libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-core_2.10" % "7.0.0-M7",
  "org.scalaz" % "scalaz-iteratee_2.10" % "7.0.0-M7",
  "org.specs2" %% "specs2" % "1.12.3" % "test",
  "org.apache.commons" % "commons-io" % "1.3.2"
)

scalacOptions ++= Seq("-deprecation", "-feature")

resolvers ++= Seq(
  "sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "sonatype releases"  at "http://oss.sonatype.org/content/repositories/releases",
  "scala tools" at "http://scala-tools.org/repo-releases",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "local m2 repo" at "file://" + Path.userHome.absolutePath + "/.m2/repository"
)

// "Cloudera Maven Repository" at "https://repository.cloudera.com/content/repositories/releases/",

publishTo := Some(Resolver.file("local m2", new File( Path.userHome.absolutePath + "/.m2/repository" )))

assemblySettings

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case x => {
      val oldstrat = old(x)
      if (oldstrat == MergeStrategy.deduplicate) MergeStrategy.first
      else oldstrat
    }
  }
}

