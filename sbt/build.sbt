val ParallelTestsLimit = 1 //math.max(1, sys.runtime.availableProcessors / 2)
val commonSettings = List(
  organization := "com.sos-berlin.jobscheduler.engine",
  organizationName := "SOS Software GmbH, Berlin",
  organizationHomepage := Some(url("https://www.sos-berlin.com")),
  scalaVersion := Libraries.scalaVersion,
  logBuffered in Test := false,
  javacOptions in Compile ++= List("-encoding", "UTF-8", "-source", "1.8"),  // This is for javadoc, too
  javacOptions in (Compile, compile) ++= List("-target", "1.8", "-deprecation", "-Xlint:all", "-Xlint:-serial"),
  javaOptions in Test += s"-Dlogback.configurationFile=${baseDirectory.value}/project/logback.xml",  // Does not work ???
  parallelExecution in Test := false,
  fork in Test := true,
  concurrentRestrictions in Global += Tags.limit(Tags.Test, ParallelTestsLimit))

resolvers += Resolver.mavenLocal

lazy val jobscheduler = (project in file("."))
  .aggregate(
    agent,
    base,
    common,
    data,
    `agent-client`,
    `agent-data`,
    `agent-main`,
    `agent-test`,
    `http-client`,
    `http-server`,
    `engine-job-api`,
    minicom,
    taskserver,
    `taskserver-dotnet`,
    `taskserver-moduleapi`,
    tunnel,
    `tunnel-data`)
  .settings(commonSettings: _*)
  .settings(publishM2 := {})  // This project is only a build wrapper

lazy val base = project
  .settings(commonSettings: _*)
  .settings {
    import Libraries._
    libraryDependencies ++=
      scalaXml ++
      sprayJson ++
      javaxAnnotations % "compile" ++
      scalaTest % "test"
  }

lazy val data = project.dependsOn(base)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies +=
      Libraries.scalaTest % "test")

lazy val common = project.dependsOn(base, data)
  .settings(commonSettings: _*)
  .settings {
    import Libraries._
    libraryDependencies ++=
      scalaXml ++
      scalactic ++
      scalaLogging ++
      javaxInject ++
      guice ++
      typesafeConfig ++
      akkaActor ++
      sprayCan ++
      sprayHttpx ++
      sprayRouting ++
      sprayJson ++
      snakeYaml ++
      guava ++
      intelliJAnnotations % "compile" ++
      javaxAnnotations % "compile" ++
      scalaTest % "test" ++
      sprayTestkit % "test" ++
      mockito % "test" ++
      logbackClassic % "test" ++
      Nil
    }
  .enablePlugins(GitVersioning)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoKeys := Seq[BuildInfoKey](
      "buildVersion" → VersionFormatter.buildVersion(
        version = version.value,
        versionCommitHash = git.gitHeadCommit.value,
        branch = git.gitCurrentBranch.value)),
    buildInfoPackage := "com.sos.scheduler.engine.common")

lazy val agent = project.dependsOn(`agent-data`, common, data, taskserver, tunnel)
  .settings(commonSettings: _*)
  .settings(description := "JobScheduler Agent")
  .settings {
    import Libraries._
    libraryDependencies ++=
      scalaXml ++
      guava ++
      javaxAnnotations % "compile" ++
      sprayJson ++
      akkaActor ++
      akkaSlf4j ++
      sprayCan ++
      sprayHttp ++
      sprayRouting ++
      sprayClient ++
      sprayTestkit % "test" ++
      intelliJAnnotations % "compile" ++
      scalactic ++
      guice ++
      mockito % "test" ++
      scalaTest % "test" ++
      logbackClassic % "test"
  }

lazy val `agent-client` = project.dependsOn(data, `tunnel-data`, common, `agent-test`/*test only!!!*/)
  .settings(commonSettings: _*)
  .settings(description := "JobScheduler Agent - Client")
  .settings {
    import Libraries._
    libraryDependencies ++=
      guice ++
      akkaActor ++
      sprayHttp ++
      sprayClient ++
      sprayJson ++
      scalaTest % "test" ++
      logbackClassic
  }

lazy val `agent-data` = project.dependsOn(`tunnel-data`, common, data)
  .settings(commonSettings: _*)
  .settings(description := "JobScheduler Agent - Value Classes")
  .settings {
    import Libraries._
    libraryDependencies ++=
      scalaXml ++
      guava ++
      sprayJson ++
      javaxAnnotations % "compile" ++
      intelliJAnnotations % "compile" ++
      scalaTest % "test" ++
      logbackClassic % "test"
  }

lazy val `agent-main` = project.dependsOn(
  agent, `agent-client`)
  .settings(commonSettings: _*)
  .settings {
    import Libraries._
    libraryDependencies ++=
      scalaTest % "test" ++
      logbackClassic % "test"
  }

lazy val `agent-test` = project.dependsOn(
  agent, common)
  .settings(commonSettings: _*)
  .settings {
    import Libraries._
    libraryDependencies ++=
      scalaTest ++
      logbackClassic % "test"
  }

lazy val `http-client` = project.dependsOn(common, data)
  .settings(commonSettings: _*)
  .settings {
    import Libraries._
    libraryDependencies ++=
      sprayJson ++
      sprayRouting ++
      sprayClient ++
      akkaActor ++
      akkaSlf4j ++
      scalactic ++
      intelliJAnnotations % "compile" ++
      sprayTestkit % "test" ++
      scalaTest % "test" ++
      logbackClassic % "test"
  }

lazy val `http-server` = project.dependsOn(`http-client`, common, data)
  .settings(commonSettings: _*)
  .settings {
    import Libraries._
    libraryDependencies ++=
      sprayJson ++
      sprayRouting ++
      sprayHttp ++
      sprayTestkit % "test" ++
      akkaActor ++
      akkaSlf4j ++
      scalactic ++
      scalaTest % "test" ++
      logbackClassic % "test"
  }

lazy val `engine-job-api` = project.dependsOn(common)
  .settings(commonSettings: _*)
  .settings(
    description := "JobScheduler Java Job API",
    crossPaths := false)  // No Scala binary "_2.11" version in artifact name
  .settings {
    import Libraries._
    libraryDependencies ++=
      guava ++
      javaxAnnotations % "compile" ++
      groovy % "test" ++
      apacheCommonsBeanutils % "test" ++
      reflections % "test" ++
      scalaTest % "test" ++
      logbackClassic % "test"
  }

lazy val minicom = project.dependsOn(common, `engine-job-api`)
  .settings(commonSettings: _*)
  .settings {
    import Libraries._
    libraryDependencies ++=
      guava ++
      scalaXml ++
      javaxAnnotations % "compile" ++
      intelliJAnnotations % "compile" ++
      mockito % "test" ++
      scalaTest % "test" ++
      logbackClassic % "test"
  }

lazy val tunnel = project.dependsOn(`tunnel-data`, `http-server`, common, data)
  .settings(commonSettings: _*)
  .settings(description := "HTTP TCP Tunnel for JobScheduler API RPC")
  .settings {
    import Libraries._
    libraryDependencies ++=
      sprayJson ++
      sprayRouting ++
      sprayHttp ++
      sprayClient ++
      akkaActor ++
      akkaAgent ++
      akkaSlf4j ++
      scalactic ++
      intelliJAnnotations % "compile" ++
      scalaTest % "test" ++
      logbackClassic % "test"
  }

lazy val `tunnel-data` = project.dependsOn(common, data, `http-server`/*HeartbeatView is here*/)
  .settings(commonSettings: _*)
  .settings(description := "HTTP TCP Tunnel for JobScheduler API RPC - value classes")
  .settings {
    import Libraries._
    libraryDependencies ++=
      scalaTest % "test" ++
      logbackClassic % "test"
  }

lazy val taskserver  = project.dependsOn(
    `taskserver-moduleapi`,
    `taskserver-dotnet`,
    `tunnel-data`,
    minicom,
    `agent-data`,
    common,
    data)
  .settings(commonSettings: _*)
  .settings {
    import Libraries._
    libraryDependencies ++=
      scalaXml ++
      akkaActor ++
      akkaSlf4j ++
      scalactic ++
      guava ++
      mockito % "test" ++
      scalaTest % "test" ++
      logbackClassic % "test"
  }

lazy val `taskserver-moduleapi` = project.dependsOn(minicom, common)
  .settings(commonSettings: _*)
  .settings {
    import Libraries._
    libraryDependencies ++=
      scalaTest % "test" ++
      logbackClassic % "test"
  }

lazy val `taskserver-dotnet` = project.dependsOn(`taskserver-moduleapi`, `engine-job-api`, common)
  .settings(commonSettings: _*)
  .settings {
    import Libraries._
    libraryDependencies ++=
      javaxAnnotations % "compile" ++
      "net.sf.jni4net" % "jni4net.j" % "0.8.8.0" ++
      mockito % "test" ++
      scalaTest % "test" ++
      logbackClassic % "test"
  }
