import sbt._
import scala.language.implicitConversions

//noinspection TypeAnnotation
object Dependencies
{
  val scalaVersion = "2.13.5"

  val akkaVersion = "2.6.14"
  val akkaHttpVersion = "10.2.4"
  val slf4jVersion = "1.7.30"  // See also plugins.sbt
  val log4jVersion = "2.14.1"  // See also plugins.sbt
  val catsVersion = "2.4.2"
  val catsEffectVersion = "2.4.0"
  //val kittensVersion = "1.2.1"
  val fastparseVersion = "2.2.4"
  val circeVersion = "0.13.0"
  val scalaJava8Version = "0.9.1"
  val scalaTestVersion = "3.2.5"
  val scalaTestCheckVersion = "3.2.2.0"
  val scalaCheckVersion= "1.14.3"
  val simulacrumVersion = "0.19.0"
  val disciplineVersion = "1.0.3"
  val disciplineScalaTestVersion = "2.0.0"
  val monixVersion = "3.3.0"
  val monocleVersion = "1.5.0"
  val scribeVersion = "3.4.0"
  val scalaJsDomVersion = "1.1.0"
  val shapelessVersion = "2.3.3"
  val diffxVersion = "0.4.0"
  val reactorVersion = "3.4.4"
  val vavrVersion = "0.10.3"
  val jnaVersion = "5.8.0"

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
  val diffx               = "com.softwaremill.diffx" %% "diffx-core" % diffxVersion
  val diffxScalaTest      = "com.softwaremill.diffx" %% "diffx-scalatest" % "0.3.29"

  val javaxInject         = "javax.inject" % "javax.inject" % "1"
  val guice               = ("com.google.inject" % "guice" % "4.2.3" classifier "no_aop") :: javaxInject :: Nil

  val typesafeConfig      = "com.typesafe" % "config" % "1.4.1"
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
  val snakeYaml           = "org.yaml" % "snakeyaml" % "1.28"

  val findbugs            = "com.google.code.findbugs" % "jsr305" % "3.0.2"
  val guava               = "com.google.guava" % "guava" % "30.1.1-jre"
  val bouncyCastle        = "org.bouncycastle" % "bcpg-jdk15on" % "1.68"
  val hamcrest            = "org.hamcrest" % "hamcrest" % "2.2" ::
                            "org.hamcrest" % "hamcrest-library" % "2.2" :: Nil
  val jna                 = "net.java.dev.jna" % "jna-platform" % jnaVersion ::
                            "net.java.dev.jna" % "jna" % jnaVersion :: Nil
}
