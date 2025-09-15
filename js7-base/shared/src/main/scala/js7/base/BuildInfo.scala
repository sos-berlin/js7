package js7.base

import js7.base.io.JavaResource
import js7.base.io.JavaResource.UnknownJavaResourceProblem
import js7.base.problem.ProblemException
import js7.base.utils.Tests.isIntelliJIdea

object BuildInfo:

  val (
    version: String,
    longVersion: String,
    prettyVersion: String,
    buildId: String,
    commitId: String,
    javaVersion: String,
    javaRuntimeVersion: String)
  =
    val props =
      val resourceName = "js7/js7-engine.properties"
      try JavaResource(resourceName).toProperties
      catch
        case e @ ProblemException(UnknownJavaResourceProblem(`resourceName`)) if isIntelliJIdea =>
          throw new AssertionError(
            s"💥 Try to rebuild with sbt: $resourceName resource is missing", e)

    (props.get("build.version").nn.asInstanceOf[String],
      props.get("build.longVersion").nn.asInstanceOf[String],
      props.get("build.prettyVersion").nn.asInstanceOf[String],
      props.get("build.buildId").nn.asInstanceOf[String],
      props.get("build.commitId").nn.asInstanceOf[String],
      props.get("build.javaVersion").nn.asInstanceOf[String],
      props.get("build.javaRuntimeVersion").nn.asInstanceOf[String]
    )
