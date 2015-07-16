package com.sos.scheduler.engine.common.maven

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.utils.JavaResource
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter.ISO_DATE_TIME
import scala.collection.JavaConversions._
import MavenProperties._

final class MavenProperties(resourcePath: JavaResource) {

  private val properties: Map[String, String] =
    autoClosing(resourcePath.url.openStream()) { in ⇒
      val p = new java.util.Properties
      p.load(in)
      p.toMap
    }

  override def toString = s"$groupId:$artifactId-$version $branchAndCommitSuffix".trim

  lazy val groupId = asString("project.groupId")

  lazy val artifactId = asString("project.artifactId")

  lazy val buildVersion: String = {
    var result = version
    if (version endsWith "-SNAPSHOT") result += s" $branchAndCommitSuffix"
    result
  }

  private def branchAndCommitSuffix =
    List(versionBranch, versionCommitHash take 7, BuildDateTimeFormatter.format(buildDateTime)) filter { _.nonEmpty } mkString ("(", " ", ")")

  lazy val version: String = asString("project.version")

  lazy val versionCommitHash = asString("sourceVersion.commitHash")

  lazy val versionBranch = asString("sourceVersion.branch") match {
    case "UNKNOWN" ⇒ sys.env.getOrElse("GIT_BRANCH", "")  // Jenkins Git plugin
    case "${scmBranch}" ⇒ ""
    case o ⇒ o
  }

  lazy val buildDateTime: ZonedDateTime = {
    val repairedDateTime = asString("maven.build.timestamp") match {
      case o if o.length == 28 && o(23) == '+' ⇒ o.substring(0, 26) + ":" + o.substring(26)  // Insert a colon like "+02:00"
      case o ⇒ o
    }
    ZonedDateTime.from(ISO_DATE_TIME.parse(repairedDateTime))
  }

  private def asString(name: String): String =
    properties.getOrElse(name, throw new NoSuchElementException(s"Unknown property '$name'"))
}

object MavenProperties {
  val EngineCommonMavenProperties = new MavenProperties(JavaResource("com/sos/scheduler/engine/common/maven/maven.properties"))
  private val BuildDateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
}
