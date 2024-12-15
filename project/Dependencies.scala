import sbt.*
import sbt.CrossVersion.for3Use2_13
import scala.language.implicitConversions

//noinspection TypeAnnotation
object Dependencies
{
  val pekkoVersion = "1.1.2"
  val pekkoHttpVersion = "1.1.0"
  val slf4jVersion = "2.0.16"  // See also plugins.sbt
  val log4jVersion = "2.24.2"  // See also plugins.sbt
  val catsVersion = "2.12.0"
  val catsEffectVersion = "3.5.7"
  val catsEffectTestingVersion = catsEffectVersion
  val izumiReflectVersion = "2.3.8"
  //val kittensVersion = "1.2.1"
  val catsParseVersion = "1.0.0"
  val circeVersion = "0.14.10"
  val circeGenericExtrasVersion = "0.14.3"
  val scalaTestVersion = "3.2.19"
  val scalaTestCheckVersion = "3.2.14.0"
  val scalaCheckVersion= "1.18.1"
  val sourcecodeVersion = "0.4.1"
  val disciplineVersion = "1.5.1"
  val disciplineScalaTestVersion = "2.2.0"
  val fs2Version = "3.11.0"
  val literallyVersion = "1.2.0"
  val scribeVersion = "3.6.7" // TODO "3.8.2"
  val scalaJsDomVersion = "2.2.0"
  val softwaremillTaggingVersion = "2.3.5"
  val reactorVersion = "3.6.10"
  val vavrVersion = "0.10.4"
  val jnaVersion = "5.15.0"

  val slf4j               = "org.slf4j" % "slf4j-api" % slf4jVersion
  val slf4jNop            = "org.slf4j" % "slf4j-nop" % slf4jVersion
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
  val catsEffectTesting   = List(
    "org.typelevel" %% "cats-effect-testkit" % catsEffectTestingVersion % Test,
    "org.typelevel" %% "cats-effect-testing-scalatest" % "1.6.0" % Test)
  val tagging             = "com.softwaremill.common" %% "tagging" % softwaremillTaggingVersion

  val javaxInject         = "javax.inject" % "javax.inject" % "1"

  val typesafeConfig      = "com.typesafe" % "config" % "1.4.3"

  val pekkoActor          = "org.apache.pekko" %% "pekko-actor" % pekkoVersion cross for3Use2_13
  val pekkoStream         = "org.apache.pekko" %% "pekko-stream" % pekkoVersion cross for3Use2_13
  val pekkoSlf4j          = "org.apache.pekko" %% "pekko-slf4j" % pekkoVersion cross for3Use2_13
  val pekkoHttp           = List(
    "org.apache.pekko" %% "pekko-http" % pekkoHttpVersion cross for3Use2_13,
    pekkoStream,
    pekkoActor/*force version*/)
  val pekkoHttpTestkit    = List(
    "org.apache.pekko" %% "pekko-http-testkit" % pekkoHttpVersion cross for3Use2_13,
    "org.apache.pekko" %% "pekko-stream-testkit" % pekkoVersion/*force version*/ cross for3Use2_13)

  val circe               = "io.circe" %% "circe-core" % circeVersion ::
                            "io.circe" %% "circe-parser" % circeVersion ::
                            "io.circe" %% "circe-generic" % circeVersion :: Nil

  val intelliJAnnotations = "com.intellij" % "annotations" % "12.0"

  val findbugs            = "com.google.code.findbugs" % "jsr305" % "3.0.2"
  val bouncyCastle        = "org.bouncycastle" % "bcpg-jdk18on" % "1.79"
  val hamcrest            = "org.hamcrest" % "hamcrest" % "3.0" ::
                            "org.hamcrest" % "hamcrest-library" % "3.0" :: Nil
  val jna                 = "net.java.dev.jna" % "jna-platform" % jnaVersion ::
                            "net.java.dev.jna" % "jna" % jnaVersion :: Nil
}
