package js7.base

import js7.base.io.JavaResource

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
    val props = JavaResource("js7/js7-engine.properties").toProperties
    (props.get("build.version").nn.asInstanceOf[String],
      props.get("build.longVersion").nn.asInstanceOf[String],
      props.get("build.prettyVersion").nn.asInstanceOf[String],
      props.get("build.buildId").nn.asInstanceOf[String],
      props.get("build.commitId").nn.asInstanceOf[String],
      props.get("build.javaVersion").nn.asInstanceOf[String],
      props.get("build.javaRuntimeVersion").nn.asInstanceOf[String]
    )
