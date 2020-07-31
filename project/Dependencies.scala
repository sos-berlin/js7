import sbt._
import scala.language.implicitConversions

//noinspection TypeAnnotation
object Dependencies
{
  val scalaVersion = "2.13.3"

  val akkaVersion = "2.6.8"
  val akkaHttpVersion = "10.1.12"
  val slf4jVersion = "1.7.30"  // See also plugins.sbt
  val log4jVersion = "2.13.3"  // See also plugins.sbt
  val catsVersion = "2.1.1"
  val catsEffectVersion = "2.1.3"
  val kittensVersion = "1.2.1"
  val fastparseVersion = "2.2.4"
  val circeVersion = "0.13.0"
  val scalaTestVersion = "3.1.2"
  val scalaTestCheckVersion = "3.1.0.0"
  val scalaCheckVersion= "1.14.3"
  val simulacrumVersion = "0.19.0"
  val disciplineVersion = "1.0.1"
  val monixVersion = "3.2.2"
  val monocleVersion = "1.5.0"
  val scribeVersion = "2.7.10"
  val scalaJsDomVersion = "0.9.7"
  val sangriaVersion = "2.0.0-RC1"
  val sangriaCirceVersion = "1.3.0"
  val shapelessVersion = "2.3.3"

  val slf4j               = "org.slf4j" % "slf4j-api"    % slf4jVersion
  val slf4jNop            = "org.slf4j" % "slf4j-nop"    % slf4jVersion
//val julToSlf4J          = "org.slf4j" % "jul-to-slf4j" % slf4jVersion
//val jansi               = "org.fusesource.jansi" % "jansi" % "1.17"
  val lmaxDisruptor       = "com.lmax" % "disruptor" % "3.4.2"
  val log4j               = "org.apache.logging.log4j" % "log4j-api" % log4jVersion ::
                            "org.apache.logging.log4j" % "log4j-core" % log4jVersion ::
                            "org.apache.logging.log4j" % "log4j-slf4j-impl" % log4jVersion ::
                            /*jansi ::*/ Nil

  val scalaReflect        = "org.scala-lang" % "scala-reflect" % scalaVersion
  val scalaXml            = "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
  val scalaTest           = "org.scalatest" %% "scalatest" % scalaTestVersion ::
                            /*"org.scalatest" %% "scalatest-freespec" % scalaTestVersion ::*/ Nil
  val scalactic           = "org.scalactic" %% "scalactic" % scalaTestVersion
  val scalaCheck          = "org.scalatestplus" %% "scalacheck-1-14" % scalaTestCheckVersion ::
                            "org.scalacheck" %% "scalacheck" % scalaCheckVersion :: Nil
  val scalaLogging        = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2" :: slf4j :: Nil
  val cats                = "org.typelevel" %% "cats-core" % catsVersion
  val shapeless           = "com.chuusai" %% "shapeless" % shapelessVersion
  val diffx               = "com.softwaremill.diffx" %% "diffx-core" % "0.3.28"
  val diffxScalaTest      = "com.softwaremill.diffx" %% "diffx-scalatest" % "0.3.28"

  val javaxInject         = "javax.inject" % "javax.inject" % "1"
  val guice               = ("com.google.inject" % "guice" % "4.2.3" classifier "no_aop") :: javaxInject :: Nil

  val typesafeConfig      = "com.typesafe" % "config" % "1.3.4"
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

  val findbugs            = "com.google.code.findbugs" % "jsr305" % "3.0.2"
  val guava               = "com.google.guava" % "guava" % "29.0-jre"
  val bouncyCastle        = "org.bouncycastle" % "bcpg-jdk15on" % "1.62"
  val hamcrest            = "org.hamcrest" % "hamcrest" % "2.2" ::
                            "org.hamcrest" % "hamcrest-library" % "2.2" :: Nil
}
