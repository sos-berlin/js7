/**
  * Recommended start script: ./build (requires bash)
  *
  * Under Windows, compile engine-job-api first, to allow taskserver-dotnet accessing the class files of engine-job-api.
  * For example: ;project engine-job-api; compile; project /; compile; test
  */

import BuildUtils._

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
    buildInfoKeys := List[BuildInfoKey](version),
    buildInfoKeys := List[BuildInfoKey](
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
      "net.sf.jni4net" % "jni4net-bin" % "0.8.8.0" % "compile" ++
      "com.sos-berlin" % "jni4net.n-sos" % "0.8.8.0" % "compile" ++
      mockito % "test" ++
      scalaTest % "test" ++
      logbackClassic % "test"
  }
  .settings(
    sourceGenerators in Compile += Def.task {
      if (!isWindows) Nil
      else {
        import java.lang.ProcessBuilder.Redirect.PIPE
        import java.nio.file.Files.createDirectories
        println("+++ Preparing and calling jni4net +++")
        // engine-job-api must have been compiled before running this code !!!
        val jobApiClassesDir = baseDirectory.value / "../engine-job-api/target/classes/sos/spooler"  // How to do this right with a TaskKey???

        def provideJobApiClassFiles(): Unit = {
          val classes = listFiles(jobApiClassesDir) filter { file ⇒
            file.isFile && !Set("Job_impl.class", "Monitor_impl.class", "Spooler_program.class").contains(file.getName)
          }
          if (classes.isEmpty) sys.error(s"Expecting class files in $jobApiClassesDir")
          val dest = target.value / "jni4net-input/javaClasses/sos/spooler"
          createDirectories(dest.toPath)
          for (classFile ← classes) IO.copyFile(classFile, dest / classFile.getName)
        }

        def extractProxygen() = IO.unzip(
          from = (update in Compile).value.select().filter { _.name contains "jni4net-bin-0.8.8.0.jar" }.head,
          toDirectory = (target in Compile).value / "jni4net",
          filter = { name: String ⇒ name.startsWith("bin/proxygen.exe") || name.startsWith("lib/") })

        def extractDll() = IO.unzip(
          from = (update in Compile).value.select().filter { _.name contains "jni4net.n-sos-0.8.8.0.jar" }.head,
          toDirectory = (target in Compile).value / "jni4net_forked")

        def callPowershellScript(): Unit = {
          val scriptFile = baseDirectory.value / "src/main/scripts/Generate-Jni4Net.ps1"
          executeWindowsCmd(
            command = s"""powershell -NonInteractive -noprofile -executionpolicy bypass "& '$scriptFile' '${target.value}' '${crossTarget.value}/classes'" <NUL""",
            name = scriptFile.toString)
        }

        def executeWindowsCmd(command: String, name: String): Unit = {
          val cmd = sys.env.get("ComSpec") orElse sys.env.get("COMSPEC"/*cygwin*/) getOrElse """C:\Windows\system32\cmd.exe"""
          val processBuilder = new java.lang.ProcessBuilder(cmd, "/C", command)
            .inheritIO().redirectInput(PIPE).directory(baseDirectory.value)
          for (o ← javaHome.value) processBuilder.environment.put("JAVA_HOME", o.toString)
          val process = processBuilder.start()
          process.getOutputStream.close()
          val exitValue = process.waitFor()
          if (exitValue != 0) sys.error(s"Process '$name' terminates with exitValue=$exitValue")
        }

        provideJobApiClassFiles()
        extractProxygen()
        extractDll()
        callPowershellScript()

        listFiles(target.value / "jni4net-build/jvm/sos/spooler")  // Generated Java source files
      }
    }.taskValue)
