import sbt._

object Dependencies{
  val resolvers = Seq(
    "maven2" at "http://repo.maven.apache.org/maven2",
    "sonatype releases"  at "http://oss.sonatype.org/content/repositories/releases",
    "sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",

    "scala-tools" at "http://scala-tools.org/repo-releases",

    "typesafe" at "http://repo.typesafe.com/typesafe/releases/",

    "conjars" at "http://conjars.org/repo",

    "scalaz bintray releases" at "http://dl.bintray.com/scalaz/releases",

    "spray io" at "http://repo.spray.io/",

    "cloudera" at "https://repository.cloudera.com/content/repositories/releases/"
  )

  val akkaVersion = "2.3.0"

  val common = Seq(
    "org.scalaz" % "scalaz-core_2.10" % "7.0.6",
    "org.scalaz" % "scalaz-concurrent_2.10" % "7.0.6",
    "org.scalaz.stream" % "scalaz-stream_2.10" % "0.4.1",
    "com.fasterxml.jackson.module" % "jackson-module-scala_2.10" % "2.3.2",

    "ch.qos.logback" % "logback-classic" % "1.0.9",
    "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",

    "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
    "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
  )

  val util = common ++ Seq(
    "org.scalaz" % "scalaz-effect_2.10" % "7.0.5",
    "com.github.nscala-time" %% "nscala-time" % "0.2.0",
    "commons-io" % "commons-io" % "2.4",
    "io.argonaut" %% "argonaut" % "6.0.2",
    "com.chuusai" % "shapeless_2.10.4" % "2.0.0"
  )

  val collection = common

  val async = common ++ Seq(
    "io.netty" % "netty-all" % "4.0.17.Final",
    "com.netflix.rxjava" % "rxjava-scala" % "0.17.1",
    "com.typesafe.play" %% "play-iteratees" % "2.2.0",
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-agent" % akkaVersion,
    "com.rabbitmq" % "amqp-client" % "3.3.4",
    "com.github.nscala-time" %% "nscala-time" % "0.2.0"
  )

  val aggregate = common ++ Seq(
    "com.googlecode.javaewah" % "JavaEWAH" % "0.6.6",
    "org.apache.commons" % "commons-math3" % "3.2",
    "org.spire-math" %% "spire" % "0.6.0"
  )
}
