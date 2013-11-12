import sbt._

object Dependencies{
  val resolvers = Seq(
    "sonatype releases"  at "http://oss.sonatype.org/content/repositories/releases",
    "sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",

    "scala-tools" at "http://scala-tools.org/repo-releases",

    "typesafe" at "http://repo.typesafe.com/typesafe/releases/",

    "conjars" at "http://conjars.org/repo",

    "spray io" at "http://repo.spray.io/",

    "cloudera" at "https://repository.cloudera.com/content/repositories/releases/"
  )

  val common = Seq(
    "org.scalaz" % "scalaz-core_2.10" % "7.0.4",
    "org.scalaz" % "scalaz-effect_2.10" % "7.0.4",

    "com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full,

    "com.github.nscala-time" %% "nscala-time" % "0.2.0",

    "ch.qos.logback" % "logback-classic" % "1.0.9",
    "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",

    "org.specs2" %% "specs2" % "2.3.3" % "test",
    "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
  )

  val util = common ++ Seq(
    "org.apache.commons" % "commons-io" % "1.3.2",
    "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.3.2"
  )

  val aggregate = common ++ Seq(
    "com.googlecode.javaewah" % "JavaEWAH" % "0.6.6",
    "org.apache.commons" % "commons-math3" % "3.2",
    "org.la4j" % "la4j" % "0.4.5",
    "org.spire-math" %% "spire" % "0.6.0"
  )

  val doo = common ++ Seq(
    "com.nicta" %% "scoobi" % "0.8.0-cdh3-SNAPSHOT",
    "org.scala-lang" %% "scala-pickling" % "0.8.0-SNAPSHOT"
  )

}

