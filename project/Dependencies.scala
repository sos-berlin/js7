import sbt._
import scala.collection.immutable.Seq
import scala.language.implicitConversions

//noinspection TypeAnnotation
object Dependencies {
  val scalaVersion = "2.12.5"

  val akkaVersion = "2.5.11"
  val akkaHttpVersion = "10.1.1"
  val slf4jVersion = "1.7.25"
  val log4jVersion = "2.11.0"
  val catsVersion = "1.1.0"
  val catsEffectVersion = "0.10"
  val fastparseVersion = "1.0.0"
  val kittensVersion = "1.0.0-RC3"
  val circeVersion = "0.9.3"
  val scalaTestVersion = "3.0.5"
  val simulacrumVersion = "0.12.0"
  val disciplineVersion = "0.8"
  val monixVersion = "2.3.3"
  val monocleVersion = "1.5.0"
  val scalaJsDomVersion = "0.9.5"
  val scajaJsJQueryVersion = "0.9.3"
  val bootstrapVersion = "4.0.0"
  val sangriaVersion = "1.4.0"
  val sangriaCirceVersion = "1.2.1"

  val slf4j               = "org.slf4j" % "slf4j-api"    % slf4jVersion
  val slf4jNop            = "org.slf4j" % "slf4j-nop"    % slf4jVersion
//val julToSlf4J          = "org.slf4j" % "jul-to-slf4j" % slf4jVersion
  val log4jApi            = "org.apache.logging.log4j" % "log4j-api" % log4jVersion
  val log4jCore           = "org.apache.logging.log4j" % "log4j-core" % log4jVersion
  val log4jSlf4j          = "org.apache.logging.log4j" % "log4j-slf4j-impl" % log4jVersion
  val jansi               = "org.fusesource.jansi" % "jansi" % "1.17"
  val lmaxDisruptor       = "com.lmax" % "disruptor" % "3.4.1"
  val log4j               = log4jSlf4j :: log4jApi :: log4jCore :: lmaxDisruptor /*:: jansi*/ :: Nil

  val scalaReflect        = "org.scala-lang" % "scala-reflect" % scalaVersion
  val scalaXml            = "org.scala-lang.modules" %% "scala-xml" % "1.1.0"
  val scalaTest           = "org.scalatest" %% "scalatest" % scalaTestVersion
  val scalactic           = "org.scalactic" %% "scalactic" % scalaTestVersion
  val scalaCheck          = "org.scalacheck" %% "scalacheck" % "1.13.5"
  val scalaLogging        = "com.typesafe.scala-logging" %% "scala-logging" % "3.8.0" :: slf4j :: Nil
  val cats                = "org.typelevel" %% "cats-core" % catsVersion

  val tagging             = "com.softwaremill.common" %% "tagging" % "2.2.0"
  val javaxInject         = "javax.inject" % "javax.inject" % "1"
  val guice               = ("com.google.inject" % "guice" % "4.2.0" classifier "no_aop") :: javaxInject :: Nil

  val typesafeConfig      = "com.typesafe" % "config" % "1.3.3"
  val akkaActor           = "com.typesafe.akka" %% "akka-actor" % akkaVersion
  val akkaStream          = "com.typesafe.akka" %% "akka-stream" % akkaVersion
  val akkaSlf4j           = "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
  val akkaHttp            = "com.typesafe.akka" %% "akka-http" % akkaHttpVersion :: akkaStream :: akkaActor/*force version*/ :: Nil
  val akkaHttpTestkit     = "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion

  val circe               = "io.circe" %% "circe-core" % circeVersion ::
                            "io.circe" %% "circe-parser" % circeVersion ::
                            "io.circe" %% "circe-generic" % circeVersion :: Nil

  val fastparse           = "com.lihaoyi" %% "fastparse" % fastparseVersion
  val scalaTags           = "com.lihaoyi" %% "scalatags" % "0.6.7"

  val mockito             = "org.mockito" % "mockito-core" % "1.10.19"
  val intelliJAnnotations = "com.intellij" % "annotations" % "12.0"
  val snakeYaml           = "org.yaml" % "snakeyaml" % "1.20"

  val javaxAnnotations    = "com.google.code.findbugs" % "jsr305" % "1.3.9"  // Everyone uses this version
  val guava               = "com.google.guava" % "guava" % "24.1-jre"
  val apacheCommonsBeanutils = "commons-beanutils" % "commons-beanutils" % "1.9.2"
  val reflections         = "org.reflections" % "reflections" % "0.9.11"
  val groovy              = "org.codehaus.groovy" % "groovy" % "1.8.6"

  object webjars {
    val bootstrap = "org.webjars.npm" % "bootstrap" % bootstrapVersion
    val materialIcons = "org.webjars" % "material-design-icons" % "3.0.1"
  }

  implicit def singleModuleIDToList(o: sbt.ModuleID): List[ModuleID] = o :: Nil

  implicit final class PercentModuleIDSeq(private val delegate: Seq[sbt.ModuleID]) extends AnyVal {
    def %(configurations: String) = delegate map { _ % configurations }
  }
}
