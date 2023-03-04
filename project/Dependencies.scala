import sbt.CrossVersion.for3Use2_13
import sbt._
import scala.language.implicitConversions

//noinspection TypeAnnotation
object Dependencies
{
  val scalaVersion = "2.13.10"

  val akkaVersion = "2.6.20"      // Do not update to v2.7, due to restrictive Akka licences!
  val akkaHttpVersion = "10.2.10" // Do not update to v10.4, due to restrictive Akka licences!
  val slf4jVersion = "2.0.5"  // See also plugins.sbt
  val log4jVersion = "2.20.0"  // See also plugins.sbt
  val catsVersion = "2.9.0"
  val catsEffectVersion = "2.5.5"
  val izumiReflectVersion = "2.1.3"
  //val kittensVersion = "1.2.1"
  val catsParseVersion = "0.3.8"
  val fastparseVersion = "2.3.3"
  val circeVersion = "0.14.5"
  val circeGenericExtrasVersion = "0.14.1"
  val scalaTestVersion = "3.2.15"
  val scalaTestCheckVersion = "3.2.14.0"
  val scalaCheckVersion= "1.17.0"
  val sourcecodeVersion = "0.3.0"
  val disciplineVersion = "1.5.1"
  val disciplineScalaTestVersion = "2.2.0"
  val fs2Version = "2.5.11"
  val monixVersion = "3.4.1"
  val monocleVersion = "1.5.0"
  val scribeVersion = "3.6.7" // TODO "3.8.2"
  val scalaJsDomVersion = "2.2.0"
  val softwaremillTaggingVersion = "2.3.4"
  val diffxVersion = "0.8.2"
  val reactorVersion = "3.5.3"
  val vavrVersion = "0.10.4"
  val jnaVersion = "5.12.1"

  val slf4j               = "org.slf4j" % "slf4j-api"    % slf4jVersion
  val slf4jNop            = "org.slf4j" % "slf4j-nop"    % slf4jVersion
  val lmaxDisruptor       = "com.lmax" % "disruptor" % "3.4.4"
  val log4j               = "org.apache.logging.log4j" % "log4j-api" % log4jVersion ::
                            "org.apache.logging.log4j" % "log4j-core" % log4jVersion ::
                            "org.apache.logging.log4j" % "log4j-slf4j2-impl" % log4jVersion ::
                            /*jansi ::*/ Nil

  val scalaTest           = "org.scalatest" %% "scalatest" % scalaTestVersion :: Nil
  val scalactic           = "org.scalactic" %% "scalactic" % scalaTestVersion
  val scalaCheck          = "org.scalatestplus" %% "scalacheck-1-16" % scalaTestCheckVersion ::
                            "org.scalacheck" %% "scalacheck" % scalaCheckVersion :: Nil
  val scalaLogging        = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5" :: slf4j :: Nil
  val cats                = "org.typelevel" %% "cats-core" % catsVersion
  val tagging             = "com.softwaremill.common" %% "tagging" % softwaremillTaggingVersion
  val diffx               = "com.softwaremill.diffx" %% "diffx-core" % diffxVersion
  val diffxScalaTest      = "com.softwaremill.diffx" %% "diffx-scalatest-should" % diffxVersion

  val javaxInject         = "javax.inject" % "javax.inject" % "1"
  val guice               = ("com.google.inject" % "guice" % "5.1.0") :: javaxInject :: Nil

  val typesafeConfig      = "com.typesafe" % "config" % "1.4.2"

  val akkaActor           = "com.typesafe.akka" %% "akka-actor" % akkaVersion cross for3Use2_13
  val akkaStream          = "com.typesafe.akka" %% "akka-stream" % akkaVersion cross for3Use2_13
  val akkaSlf4j           = "com.typesafe.akka" %% "akka-slf4j" % akkaVersion cross for3Use2_13
  val akkaHttp            = List(
    "com.typesafe.akka" %% "akka-http" % akkaHttpVersion cross for3Use2_13,
    akkaStream,
    akkaActor/*force version*/)
  val akkaHttpTestkit     = List(
    "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion cross for3Use2_13,
    "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion/*force version*/ cross for3Use2_13)

  val circe               = "io.circe" %% "circe-core" % circeVersion ::
                            "io.circe" %% "circe-parser" % circeVersion ::
                            "io.circe" %% "circe-generic" % circeVersion :: Nil

  val fastparse           = "com.lihaoyi" %% "fastparse" % fastparseVersion

  val intelliJAnnotations = "com.intellij" % "annotations" % "12.0"

  val findbugs            = "com.google.code.findbugs" % "jsr305" % "3.0.2"
  //val guava               = "com.google.guava" % "guava" % "30.1.1-jre"
  val bouncyCastle        = "org.bouncycastle" % "bcpg-jdk15on" % "1.70"
  val hamcrest            = "org.hamcrest" % "hamcrest" % "2.2" ::
                            "org.hamcrest" % "hamcrest-library" % "2.2" :: Nil
  val jna                 = "net.java.dev.jna" % "jna-platform" % jnaVersion ::
                            "net.java.dev.jna" % "jna" % jnaVersion :: Nil
}
