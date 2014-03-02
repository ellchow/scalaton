import sbt._

object Dependencies{
  val resolvers = Seq(
    "sonatype releases"  at "http://oss.sonatype.org/content/repositories/releases",
    "sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",

    "scala-tools" at "http://scala-tools.org/repo-releases",

    "typesafe" at "http://repo.typesafe.com/typesafe/releases/",

    "conjars" at "http://conjars.org/repo",

    "scalaz bintray releases" at "http://dl.bintray.com/scalaz/releases",

    "spray io" at "http://repo.spray.io/",

    "cloudera" at "https://repository.cloudera.com/content/repositories/releases/"
  )

  val common = Seq(
    "org.scalaz" % "scalaz-core_2.10" % "7.0.5",

    "ch.qos.logback" % "logback-classic" % "1.0.9",
    "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",

    "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
    "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
  )

  val util = common ++ Seq(
    "org.scalaz" % "scalaz-effect_2.10" % "7.0.5",
    "com.github.nscala-time" %% "nscala-time" % "0.2.0",
    "org.apache.commons" % "commons-io" % "1.3.2",
    "io.argonaut" %% "argonaut" % "6.0.2",
    "com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full
  )

  val collection = common ++ Seq(
    "io.argonaut" %% "argonaut" % "6.0.2"
  )

  val stream = common ++ Seq(
    "io.argonaut" %% "argonaut" % "6.0.2",
    "org.scalaz.stream" %% "scalaz-stream" % "0.3.1"
  )

  val aggregate = common ++ Seq(
    "com.googlecode.javaewah" % "JavaEWAH" % "0.6.6",
    "org.apache.commons" % "commons-math3" % "3.2",
    "org.spire-math" %% "spire" % "0.6.0"
  )

}
