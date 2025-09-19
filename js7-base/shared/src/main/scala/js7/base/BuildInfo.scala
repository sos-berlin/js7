package js7.base

import java.util.Properties
import js7.base.io.JavaResource
import js7.base.io.JavaResource.UnknownJavaResourceProblem
import js7.base.problem.ProblemException
import js7.base.utils.Tests.isIntelliJIdea

object BuildInfo:

  private val props: Properties =
    val resourceName = "js7/js7-engine.properties"
    try JavaResource(resourceName).toProperties
    catch
      case e @ ProblemException(UnknownJavaResourceProblem(`resourceName`)) if isIntelliJIdea =>
        throw new AssertionError(
          s"💥 Try to rebuild with sbt: $resourceName resource is missing", e)

  lazy val version: String = prop
  lazy val longVersion: String = prop
  lazy val prettyVersion: String = prop
  lazy val buildId: String = prop
  //lazy val commitId: String = prop
  //lazy val javaVersion: Int = prop.toInt
  //lazy val javaRuntimeVersion: String = prop
  lazy val catsEffectVersion: String = prop

  private def prop(using name: sourcecode.Name): String =
    props.get(s"build.${name.value}").nn.asInstanceOf[String]
