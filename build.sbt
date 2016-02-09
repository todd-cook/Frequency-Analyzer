
  organization := "com.wordtrellis"
  name := "frequency-analyzer"
  version := "1.0.0-SNAPSHOT"
  scalaVersion := "2.11.7"
  scalacOptions += "-deprecation"

libraryDependencies ++= Seq(
  "org.codehaus.jackson" % "jackson-core-asl" % "1.6.1",
  "org.scala-lang" % "scala-library" % "2.11.7"  ,
  "org.scala-lang" %  "scala-swing" % "2.11.0-M7"  ,
  "com.miglayout" % "miglayout-swing" % "5.0",
  "org.scalactic" % "scalactic_2.11" % "2.2.6",
  "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test",
  "junit" % "junit" % "4.8.2" % "test" ,
  "org.slf4j" % "slf4j-api" % "1.6.1",
  "org.slf4j" % "slf4j-simple" % "1.6.1",
  "com.typesafe.akka" % "akka-actor_2.11" % "2.4.1",
  "com.typesafe.akka" % "akka-slf4j_2.11" % "2.4.1",
  "ch.qos.logback" % "logback-classic" % "1.0.9")

  lazy val logback = "ch.qos.logback" % "logback-classic" % "1.0.9"
