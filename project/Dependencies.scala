import sbt._
import scala.collection.immutable.Seq
import scala.language.implicitConversions

//noinspection TypeAnnotation
object Dependencies {
  val scalaVersion = "2.12.3"

  val akkaVersion = "2.5.4"
  val akkaHttpVersion = "10.0.10"
  val slf4jVersion = "1.7.25"
  val log4jVersion = "2.9.0"

  val slf4j               = "org.slf4j" % "slf4j-api"    % slf4jVersion
  val slf4jNop            = "org.slf4j" % "slf4j-nop"    % slf4jVersion
//val julToSlf4J          = "org.slf4j" % "jul-to-slf4j" % slf4jVersion
  val log4jApi            = "org.apache.logging.log4j" % "log4j-api" % log4jVersion
  val log4jCore           = "org.apache.logging.log4j" % "log4j-core" % log4jVersion
  val log4jSlf4j          = "org.apache.logging.log4j" % "log4j-slf4j-impl" % log4jVersion
  val jansi               = "org.fusesource.jansi" % "jansi" % "1.14"
  val lmaxDisruptor       = "com.lmax" % "disruptor" % "3.3.6"
  val log4j               = log4jSlf4j :: log4jApi :: log4jCore :: lmaxDisruptor /*:: jansi*/ :: Nil

  val scalaReflect        = "org.scala-lang" % "scala-reflect" % scalaVersion
  val scalaXml            = "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
  val scalaTest           = "org.scalatest" %% "scalatest" % "3.0.1"
  val scalactic           = "org.scalactic" %% "scalactic" % "3.0.1"
  val scalaLogging        = "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0" :: slf4j :: Nil

  val tagging             = "com.softwaremill.common" %% "tagging" % "2.2.0"
  val javaxInject         = "javax.inject" % "javax.inject" % "1"
  val guice               = "com.google.inject" % "guice" % "4.1.0" :: javaxInject :: Nil

  val typesafeConfig      = "com.typesafe" % "config" % "1.3.1"
  val akkaActor           = "com.typesafe.akka" %% "akka-actor" % akkaVersion
  val akkaStream          = "com.typesafe.akka" %% "akka-stream" % akkaVersion
  val akkaAgent           = "com.typesafe.akka" %% "akka-agent" % akkaVersion
  val akkaSlf4j           = "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
  val akkaHttp            = "com.typesafe.akka" %% "akka-http" % akkaHttpVersion :: akkaStream :: akkaActor/*force version*/ :: Nil
  val akkaHttpSprayJson   = "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion
  val akkaHttpTestkit     = "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion

  val sprayJson           = "io.spray" %% "spray-json" % "1.3.3" :: scalaReflect :: Nil

  val scalaTags           = "com.lihaoyi" %% "scalatags" % "0.6.5"

  val mockito             = "org.mockito" % "mockito-core" % "1.10.19"
  val intelliJAnnotations = "com.intellij" % "annotations" % "12.0"
  val snakeYaml           = "org.yaml" % "snakeyaml" % "1.18"

  val javaxAnnotations    = "com.google.code.findbugs" % "jsr305" % "3.0.0"
  val guava               = "com.google.guava" % "guava" % "21.0"
  val apacheCommonsBeanutils = "commons-beanutils" % "commons-beanutils" % "1.9.2"
  val reflections         = "org.reflections" % "reflections" % "0.9.9"
  val groovy              = "org.codehaus.groovy" % "groovy" % "1.8.6"

  object webjars {
    val bootstrap = "org.webjars" % "bootstrap" % "3.3.6"
    val jQuery    = "org.webjars" % "jquery" % "2.2.4"
  }

  implicit def singleModuleIDToList(o: sbt.ModuleID): List[ModuleID] = o :: Nil

  implicit class PercentModuleIDSeq(val delegate: Seq[sbt.ModuleID]) extends AnyVal {
    def %(configurations: String) = delegate map { _ % configurations }
  }
}
