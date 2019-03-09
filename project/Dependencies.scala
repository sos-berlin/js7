import sbt._
import scala.collection.immutable.Seq
import scala.language.implicitConversions

//noinspection TypeAnnotation
object Dependencies
{
  val scalaVersion = "2.12.8"

  val akkaVersion = "2.5.21"
  val akkaHttpVersion = "10.1.7"
  val slf4jVersion = "1.7.26"
  val log4jVersion = "2.11.2"
  val catsVersion = "1.6.0"
  val catsEffectVersion = "1.2.0"
  val fastparseVersion = "2.1.0"
  val kittensVersion = "1.2.0"
  val circeVersion = "0.11.1"
  val scalaTestVersion = "3.0.5"
  val simulacrumVersion = "0.12.0"
  val disciplineVersion = "0.8"  //"0.11.0"
  val monixVersion = "3.0.0-RC2"
  val monocleVersion = "1.5.0"
  val scalaJsDomVersion = "0.9.6"
  val sangriaVersion = "1.4.2"
  val sangriaCirceVersion = "1.2.1"

  val slf4j               = "org.slf4j" % "slf4j-api"    % slf4jVersion
  val slf4jNop            = "org.slf4j" % "slf4j-nop"    % slf4jVersion
//val julToSlf4J          = "org.slf4j" % "jul-to-slf4j" % slf4jVersion
  val log4jApi            = "org.apache.logging.log4j" % "log4j-api" % log4jVersion
  val log4jCore           = "org.apache.logging.log4j" % "log4j-core" % log4jVersion
  val log4jSlf4j          = "org.apache.logging.log4j" % "log4j-slf4j-impl" % log4jVersion
  val jansi               = "org.fusesource.jansi" % "jansi" % "1.17"
  val lmaxDisruptor       = "com.lmax" % "disruptor" % "3.4.2"
  val log4j               = log4jSlf4j :: log4jApi :: log4jCore :: lmaxDisruptor /*:: jansi*/ :: Nil

  val scalaReflect        = "org.scala-lang" % "scala-reflect" % scalaVersion
  val scalaXml            = "org.scala-lang.modules" %% "scala-xml" % "1.1.1"
  val scalaTest           = "org.scalatest" %% "scalatest" % scalaTestVersion
  val scalactic           = "org.scalactic" %% "scalactic" % scalaTestVersion
  val scalaCheck          = "org.scalacheck" %% "scalacheck" % "1.14.0"
  val scalaLogging        = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2" :: slf4j :: Nil
  val cats                = "org.typelevel" %% "cats-core" % catsVersion

  val javaxInject         = "javax.inject" % "javax.inject" % "1"
  val guice               = ("com.google.inject" % "guice" % "4.2.2" classifier "no_aop") :: javaxInject :: Nil

  val typesafeConfig      = "com.typesafe" % "config" % "1.3.3"
  val akkaActor           = "com.typesafe.akka" %% "akka-actor" % akkaVersion
  val akkaStream          = "com.typesafe.akka" %% "akka-stream" % akkaVersion
  val akkaSlf4j           = "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
  val akkaHttp            = "com.typesafe.akka" %% "akka-http" % akkaHttpVersion :: akkaStream :: akkaActor/*force version*/ :: Nil
  val akkaHttpTestkit     = "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion ::
                            "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion/*force version*/ :: Nil

  val circe               = "io.circe" %% "circe-core" % circeVersion ::
                            "io.circe" %% "circe-parser" % circeVersion ::
                            "io.circe" %% "circe-generic" % circeVersion :: Nil

  val fastparse           = "com.lihaoyi" %% "fastparse" % fastparseVersion

  val mockito             = "org.mockito" % "mockito-core" % "1.10.19"
  val intelliJAnnotations = "com.intellij" % "annotations" % "12.0"
  val snakeYaml           = "org.yaml" % "snakeyaml" % "1.24"

  val javaxAnnotations    = "com.google.code.findbugs" % "jsr305" % "1.3.9"  // Everyone uses this version
  val guava               = "com.google.guava" % "guava" % "27.0.1-jre"

  implicit def singleModuleIDToList(o: sbt.ModuleID): List[ModuleID] = o :: Nil

  implicit final class PercentModuleIDSeq(private val delegate: Seq[sbt.ModuleID]) extends AnyVal {
    def %(configurations: String) = delegate map { _ % configurations }
  }
}
