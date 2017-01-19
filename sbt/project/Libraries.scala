import sbt._
import scala.collection.immutable.Seq
import scala.language.implicitConversions

object Libraries {
  val scalaVersion = "2.11.8"

  val akkaVersion = "2.3.15" //"2.4.7"
  val sprayVersion = "1.3.4"
  val slf4jVersion = "1.7.21"

  val scalaReflect = "org.scala-lang" % "scala-reflect" % scalaVersion
  val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.0.4"
  val scalaTest = "org.scalatest" %% "scalatest" % "2.2.4"
  val scalactic = "org.scalactic" %% "scalactic" % "2.2.4"
  val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"

  val slf4j      = "org.slf4j" % "slf4j-api"    % slf4jVersion
  val julToSlf4J = "org.slf4j" % "jul-to-slf4j" % slf4jVersion
  val logbackClassic = "ch.qos.logback" % "logback-classic" % "1.1.3"

  val javaxInject = "javax.inject" % "javax.inject" % "1"
  val guice = "com.google.inject" % "guice" % "3.0"

  val typesafeConfig = "com.typesafe" % "config" % "1.3.0"
  val akkaActor = "com.typesafe.akka" %% "akka-actor" % akkaVersion
  val sprayCan     = "io.spray" %% "spray-can"     % sprayVersion
  val sprayHttpx   = "io.spray" %% "spray-httpx"   % sprayVersion
  val sprayRouting = "io.spray" %% "spray-routing" % sprayVersion
  val sprayTestkit = "io.spray" %% "spray-testkit" % sprayVersion
  val sprayJsons = "io.spray" %% "spray-json" % "1.3.2" :: scalaReflect :: Nil

  val mockito = "org.mockito" % "mockito-core" % "1.10.19"
  val intelliJAnnotations = "com.intellij" % "annotations" % "12.0"
  val xmlApis = "xml-apis" % "xml-apis" % "1.4.01"
  val snakeYaml = "org.yaml" % "snakeyaml" % "1.15"

  val googleFindbugs = "com.google.code.findbugs" % "jsr305" % "3.0.0"
  val guava = "com.google.guava" % "guava" % "18.0"

  implicit def singleModuleIDToSeq(o: sbt.ModuleID): Seq[ModuleID] = Seq(o)
}
