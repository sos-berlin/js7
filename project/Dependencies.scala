import sbt._
import scala.language.implicitConversions

//noinspection TypeAnnotation
object Dependencies
{
  val scalaVersion = "2.13.8"

  val akkaVersion = "2.6.19"
  val akkaHttpVersion = "10.2.9"
  val slf4jVersion = "1.7.36"  // See also plugins.sbt
  val log4jVersion = "2.17.2"  // See also plugins.sbt
  val catsVersion = "2.7.0"
  val catsEffectVersion = "2.5.4"
  val izumiReflectVersion = "2.1.0"
  //val kittensVersion = "1.2.1"
  val fastparseVersion = "2.2.4"
  val circeVersion = "0.14.1"
  val scalaJava8Version = "1.0.0"
  val scalaTestVersion = "3.2.11"
  val scalaTestCheckVersion = "3.2.2.0"
  val scalaCheckVersion= "1.14.3"
  val simulacrumVersion = "0.19.0"
  val sourcecodeVersion = "0.2.8"
  val disciplineVersion = "1.0.3"
  val disciplineScalaTestVersion = "2.0.0"
  val monixVersion = "3.4.1"
  val monocleVersion = "1.5.0"
  val scribeVersion = "3.6.7" // TODO "3.8.2"
  val scalaJsDomVersion = "1.2.0"
  val shapelessVersion = "2.3.7"
  val diffxVersion = "0.7.0"
  val reactorVersion = "3.4.16"
  val vavrVersion = "0.10.4"
  val jnaVersion = "5.10.0"

  val slf4j               = "org.slf4j" % "slf4j-api"    % slf4jVersion
  val slf4jNop            = "org.slf4j" % "slf4j-nop"    % slf4jVersion
//val julToSlf4J          = "org.slf4j" % "jul-to-slf4j" % slf4jVersion
//val jansi               = "org.fusesource.jansi" % "jansi" % "1.17"
  val lmaxDisruptor       = "com.lmax" % "disruptor" % "3.4.4"
  val log4j               = "org.apache.logging.log4j" % "log4j-api" % log4jVersion ::
                            "org.apache.logging.log4j" % "log4j-core" % log4jVersion ::
                            "org.apache.logging.log4j" % "log4j-slf4j-impl" % log4jVersion ::
                            /*jansi ::*/ Nil

  val scalaReflect        = "org.scala-lang" % "scala-reflect" % scalaVersion
  val scalaTest           = "org.scalatest" %% "scalatest" % scalaTestVersion ::
                            /*"org.scalatest" %% "scalatest-freespec" % scalaTestVersion ::*/ Nil
  val scalactic           = "org.scalactic" %% "scalactic" % scalaTestVersion
  val scalaCheck          = "org.scalatestplus" %% "scalacheck-1-14" % scalaTestCheckVersion ::
                            "org.scalacheck" %% "scalacheck" % scalaCheckVersion :: Nil
  val scalaLogging        = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4" :: slf4j :: Nil
  val cats                = "org.typelevel" %% "cats-core" % catsVersion
  val shapeless           = "com.chuusai" %% "shapeless" % shapelessVersion
  val diffx               = "com.softwaremill.diffx" %% "diffx-core" % diffxVersion
  val diffxScalaTest      = "com.softwaremill.diffx" %% "diffx-scalatest-should" % diffxVersion

  val javaxInject         = "javax.inject" % "javax.inject" % "1"
  val guice               = ("com.google.inject" % "guice" % "5.1.0") :: javaxInject :: Nil

  val typesafeConfig      = "com.typesafe" % "config" % "1.4.2"
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

  val intelliJAnnotations = "com.intellij" % "annotations" % "12.0"

  val findbugs            = "com.google.code.findbugs" % "jsr305" % "3.0.2"
  //val guava               = "com.google.guava" % "guava" % "30.1.1-jre"
  val bouncyCastle        = "org.bouncycastle" % "bcpg-jdk15on" % "1.70"
  val hamcrest            = "org.hamcrest" % "hamcrest" % "2.2" ::
                            "org.hamcrest" % "hamcrest-library" % "2.2" :: Nil
  val jna                 = "net.java.dev.jna" % "jna-platform" % jnaVersion ::
                            "net.java.dev.jna" % "jna" % jnaVersion :: Nil
}
