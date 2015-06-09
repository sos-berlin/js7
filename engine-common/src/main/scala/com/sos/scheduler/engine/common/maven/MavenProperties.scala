package com.sos.scheduler.engine.common.maven

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.utils.JavaResource
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import scala.collection.JavaConversions._

final class MavenProperties(resourcePath: JavaResource) {

  private val properties: Map[String, String] =
    autoClosing(resourcePath.url.openStream()) { in ⇒
      val p = new java.util.Properties
      p.load(in)
      p.toMap
    }

  override def toString = s"$groupId:$artifactId-$version $branchAndCommitSuffix"

  lazy val groupId = asString("project.groupId")

  lazy val artifactId = asString("project.artifactId")

  lazy val buildVersion: String = {
    var result = version
    if (version endsWith "-SNAPSHOT") result += s" $branchAndCommitSuffix"
    result
  }

  private def branchAndCommitSuffix = "(" +
    (List("branch", versionBranch, versionCommitHash) filter { _.nonEmpty } mkString " ") +
    s", built $buildDateTime)"

  lazy val version: String = asString("project.version")

  lazy val versionCommitHash = asString("sourceVersion.commitHash") match {
    case "" ⇒ "(unknown-commit)"
    case o ⇒ o
  }

  lazy val versionBranch = asString("sourceVersion.branch") match {
    case o @ "UNKNOWN" ⇒ properties.getOrElse("GIT_BRANCH", o)  // Jenkins Git plugin
    case o ⇒ o
  }

  lazy val buildDateTime: DateTime = ISODateTimeFormat.dateTime.parseDateTime(asString("maven.build.timestamp"))

  private def asString(name: String): String =
    properties.getOrElse(name, throw new NoSuchElementException(s"Unknown property '$name'"))
}

object MavenProperties {
  val EngineCommonMavenProperties = new MavenProperties(JavaResource("com/sos/scheduler/engine/common/maven/maven.properties"))
}
