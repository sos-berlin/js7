import sbt._
import scala.collection.immutable.Seq
import scala.language.implicitConversions

object Libraries {
  val scalaVersion = "2.11.8"

  val akkaVersion = "2.4.16"
  val sprayVersion = "1.3.4"
  val slf4jVersion = "1.7.21"

  val slf4j               = "org.slf4j" % "slf4j-api"    % slf4jVersion
  val slf4jNop            = "org.slf4j" % "slf4j-nop"    % slf4jVersion
//val julToSlf4J          = "org.slf4j" % "jul-to-slf4j" % slf4jVersion
  val logbackClassic      = "ch.qos.logback" % "logback-classic" % "1.1.3"

  val scalaReflect        = "org.scala-lang" % "scala-reflect" % scalaVersion
  val scalaXml            = "org.scala-lang.modules" %% "scala-xml" % "1.0.4"
  val scalaTest           = "org.scalatest" %% "scalatest" % "2.2.4"
  val scalactic           = "org.scalactic" %% "scalactic" % "2.2.4"
  val scalaLogging        = "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0" :: slf4j :: Nil

  val javaxInject         = "javax.inject" % "javax.inject" % "1"
  val guice               = "com.google.inject" % "guice" % "3.0" :: javaxInject :: Nil

  val typesafeConfig      = "com.typesafe" % "config" % "1.3.0"
  val akkaActor           = "com.typesafe.akka" %% "akka-actor" % akkaVersion
  val akkaAgent           = "com.typesafe.akka" %% "akka-agent" % akkaVersion
  val akkaSlf4j           = "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
  val sprayCan            = "io.spray" %% "spray-can"     % sprayVersion
  val sprayHttp           = "io.spray" %% "spray-http"    % sprayVersion
  val sprayHttpx          = "io.spray" %% "spray-httpx"   % sprayVersion
  val sprayRouting        = "io.spray" %% "spray-routing" % sprayVersion
  val sprayClient         = "io.spray" %% "spray-client"  % sprayVersion
  val sprayTestkit        = "io.spray" %% "spray-testkit" % sprayVersion
  val sprayJson           = "io.spray" %% "spray-json" % "1.3.2" :: scalaReflect :: Nil

  val mockito             = "org.mockito" % "mockito-core" % "1.10.19"
  val intelliJAnnotations = "com.intellij" % "annotations" % "12.0"
  val snakeYaml           = "org.yaml" % "snakeyaml" % "1.15"

  val javaxAnnotations    = "com.google.code.findbugs" % "jsr305" % "3.0.0"
  val guava               = "com.google.guava" % "guava" % "18.0"
  val apacheCommonsBeanutils = "commons-beanutils" % "commons-beanutils" % "1.9.2"
  val reflections         = "org.reflections" % "reflections" % "0.9.9"
  val groovy              = "org.codehaus.groovy" % "groovy" % "1.8.6"

  implicit def singleModuleIDToSeq(o: sbt.ModuleID): Seq[ModuleID] = Seq(o)
}
