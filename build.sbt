/**
  * Install sbt from http://www.scala-sbt.org/.
  *
  * Recommended usage for CI server:
  *   sbt clean-publish
  *
  * To build locally, without publishing:
  *   sbt clean-build
  *
  * To build quickly, without running tests again:
  *   sbt "; clean ;build-quickly"
  *
  * Under Windows, Microsoft SDK is required to compile taskserver-dotnet.
  *   set WINDOWS_NET_SDK_HOME=%windir%\Microsoft.NET\Framework\v4.0.30319
  *
  * To build and publish to a repository use
  *   sbt -DpublishRepository.credentialsFile=... -DpublishRepository.name=... -DpublishRepository.uri=... clean-publish
  *   (publishRepository.name defaults to publishRepository.uri)
  *
  *   Under Windows, if system properties are not accepted, set a environment variable:
  *   set SBT_OPTS=-DpublishRepository.credentialsFile=... -DpublishRepository.name=... -DpublishRepository.uri=...
  *   sbt clean-publish
  *
  * sbt allows to preset these command line options in the environment variable SBT_OPTS. Use one line per option.
  */
import BuildUtils._
import sbt.Keys.testOptions
import sbt.librarymanagement.DependencyFilter.artifactFilter

val fastSbt = sys.env contains "FAST_SBT"

val publishRepositoryCredentialsFile = sys.props.get("publishRepository.credentialsFile") map (o ⇒ new File(o))
val publishRepositoryName            = sys.props.get("publishRepository.name")
val publishRepositoryUri             = sys.props.get("publishRepository.uri")

// Under Windows, compile engine-job-api first, to allow taskserver-dotnet accessing the class files of engine-job-api.
addCommandAlias("clean-publish"  , "; clean ;build; publish-all")
addCommandAlias("clean-build"    , "; clean; build")
addCommandAlias("build"          , "; compile-all; test-all; pack")
addCommandAlias("build-quickly"  , "; compile-all; pack")
addCommandAlias("compile-all"    , "; engine-job-api/compile; compile")
addCommandAlias("test-all"       , "; test:compile; test; ForkedTest:test")
addCommandAlias("pack"           , "universal:packageZipTarball")
addCommandAlias("publish-all"    , "universal:publish")  // Publishes artifacts too
addCommandAlias("publish-install", "; install/universal:publish; install-docker:universal:publish")

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")

val publishSettings = List(
  publishArtifact in (Compile, packageDoc) := false,
  credentials ++= publishRepositoryCredentialsFile map (o ⇒ Credentials(o)),
  publishTo := publishRepositoryUri map (uri ⇒ publishRepositoryName getOrElse uri at uri))

val commonSettings = List(
  organization := "com.sos-berlin.jobscheduler.engine2",
  organizationName := "SOS Berlin",
  organizationHomepage := Some(url("https://www.sos-berlin.com")),
  pomExtra :=
    <developers>
      <developer>
          <name>Joacim Zschimmer</name>
          <organization>{organizationName.value}</organization>
          <organizationUrl>{organizationHomepage.value.get}</organizationUrl>
      </developer>
    </developers>,
  scalaVersion := Dependencies.scalaVersion,
  javacOptions in Compile ++= List("-encoding", "UTF-8", "-source", "1.8"),  // This is for javadoc, too
  javacOptions in (Compile, compile) ++= List("-target", "1.8", "-deprecation", "-Xlint:all", "-Xlint:-serial"),
  javaOptions += s"-Dlog4j.configurationFile=../project/log4j2.xml",  // Forked only  // TODO Is there a SettingKey for project directory???
  logBuffered in Test := false,
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oFG"),  // D: Durations, F: Print full stack strace
  testOptions in ForkedTest += Tests.Argument(TestFrameworks.ScalaTest, "-oFG"),  // D: Durations, F: Print full stack strace
  logBuffered in Test := false,  // Recommended for ScalaTest
  testForkedParallel in Test := fastSbt,  // Experimental in sbt 0.13.13
  sources in (Compile, doc) := Nil, // No ScalaDoc
  test in publishM2 := {},
  // Publish
  publishArtifact in (Compile, packageDoc) := false,
  credentials += publishRepositoryCredentialsFile map (o ⇒ Credentials(o)),
  publishTo := publishRepositoryUri map (uri ⇒ publishRepositoryName getOrElse uri at uri))

val universalPluginSettings = List(
  universalArchiveOptions in (Universal, packageZipTarball) :=
    (List("--force-local") filter { _ ⇒ !isMac }) ++ (universalArchiveOptions in (Universal, packageZipTarball)).value,  // Under cygwin, tar shall not interpret C:
  universalArchiveOptions in (Universal, packageXzTarball) :=
    (List("--force-local") filter { _ ⇒ !isMac }) ++ (universalArchiveOptions in (Universal, packageXzTarball)).value)  // Under cygwin, tar shall not interpret C:

concurrentRestrictions in Global += Tags.limit(Tags.Test,  // Parallelization
  max = if (fastSbt) math.max(1, sys.runtime.availableProcessors * 3/8) else 1)

resolvers += Resolver.mavenLocal

lazy val jobscheduler = (project in file("."))
  .aggregate(
    agent,
    base,
    common,
    shared,
    data,
    `jobscheduler-docker`,
    `jobscheduler-install`,
    master,
    `agent-client`,
    `agent-data`,
    `agent-test`,
    `agent-tests`,
    `http-client`,
    `http-server`,
    `engine-job-api`,
    minicom,
    taskserver,
    `taskserver-dotnet`,
    `taskserver-moduleapi`,
    tests,
    tunnel,
    `tunnel-data`)
  .settings(skip in publish := true)

lazy val `jobscheduler-install` = project
  .dependsOn(master, agent)
  .settings(commonSettings)
  .enablePlugins(JavaAppPackaging, UniversalDeployPlugin)
  .settings(
    //skip in publish := true,  // We publish only .tgz and .zip generated by sbt-native-packager / UniversalDeployPlugin
    universalPluginSettings,
    topLevelDirectory in Universal := Some(s"jobscheduler-${version.value}"),
    mappings in Universal :=
      ((mappings in Universal).value filter { case (_, path) ⇒ (path startsWith "lib/") && !isTestJar(path stripPrefix "lib/") }) ++
        recursiveFileMapping(baseDirectory.value / "../master/src/main/resources/com/sos/jobscheduler/master/installation") ++
        recursiveFileMapping(baseDirectory.value / "../agent/src/main/resources/com/sos/jobscheduler/agent/installation") ++
        recursiveFileMapping(baseDirectory.value / "../shared/src/main/resources/com/sos/jobscheduler/shared/installation"))

lazy val `jobscheduler-docker` = project
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      log4j
  }
  .enablePlugins(JavaAppPackaging, UniversalDeployPlugin)
  .settings(
    universalPluginSettings,
    topLevelDirectory in Universal := None,
    mappings in Universal :=
      recursiveFileMapping(baseDirectory.value / "src/main/resources/com/sos/jobscheduler/install/docker", to = "build/"))

lazy val base = project
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaXml ++
      sprayJson ++
      javaxAnnotations % "compile" ++
      scalaTest % "test"
  }

lazy val data = project.dependsOn(base)
  .settings(commonSettings)
  .settings(
    libraryDependencies +=
      Dependencies.scalaTest % "test")

lazy val common = project.dependsOn(base, data)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaXml ++
      scalactic ++
      scalaLogging ++
      javaxInject ++
      guice ++
      typesafeConfig ++
      akkaHttpTestkit % "test" ++
      akkaHttp ++
      akkaHttpSprayJson ++
      akkaActor ++
      scalaTags ++
      sprayJson ++
      snakeYaml ++
      guava ++
      intelliJAnnotations % "compile" ++
      javaxAnnotations % "compile" ++
      scalaTest % "test" ++
      mockito % "test" ++
      log4j % "test" ++
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
        branch = git.gitCurrentBranch.value),
      "version" → version.value),
    buildInfoPackage := "com.sos.jobscheduler.common")

lazy val master = project.dependsOn(shared, common, `agent-client`)
  .configs(ForkedTest).settings(forkedSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      webjars.bootstrap ++
      webjars.jQuery ++
      scalaTest % "test" ++
      akkaHttpTestkit % "test" ++
      akkaHttp/*force version?*/ % "test" ++
      log4j % "test"
  }

lazy val shared = project.dependsOn(common)
  .configs(ForkedTest).settings(forkedSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val agent = project.dependsOn(`agent-data`, shared, common, data, taskserver, tunnel)
  .configs(ForkedTest).settings(forkedSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaXml ++
      guava ++
      javaxAnnotations % "compile" ++
      sprayJson ++
      akkaActor ++
      akkaStream ++
      akkaSlf4j ++
      akkaHttpTestkit % "test" ++
      akkaHttp ++
      akkaHttpSprayJson ++
      intelliJAnnotations % "compile" ++
      scalactic ++
      tagging ++
      guice ++
      mockito % "test" ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val `agent-client` = project.dependsOn(data, `tunnel-data`, common, `agent-test` % "compile->test")
  .configs(ForkedTest).settings(forkedSettings)
  .settings(commonSettings)
  .settings(description := "JobScheduler Agent - Client")
  .settings {
    import Dependencies._
    libraryDependencies ++=
      guice ++
      akkaActor ++
      akkaHttp ++
      akkaHttpSprayJson ++
      sprayJson ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val `agent-data` = project.dependsOn(`tunnel-data`, common, data)
  .configs(ForkedTest).settings(forkedSettings)
  .settings(commonSettings)
  .settings(description := "JobScheduler Agent - Value Classes")
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaXml ++
      guava ++
      sprayJson ++
      javaxAnnotations % "compile" ++
      intelliJAnnotations % "compile" ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val `agent-test` = project.dependsOn(agent, common)
  .configs(ForkedTest).settings(forkedSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaTest ++
      log4j % "test"
  }

lazy val `agent-tests` = project.dependsOn(`agent` % "test->test", `agent-client` % "test->test")
  .configs(ForkedTest).settings(forkedSettings)
  .settings(
    commonSettings,
    description := "JobScheduler Agent Tests")
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaTest % "test" ++
      mockito % "test" ++
      log4j % "test"
  }

lazy val `http-client` = project.dependsOn(common, data)
  .configs(ForkedTest).settings(forkedSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      sprayJson ++
      akkaActor ++
      akkaSlf4j ++
      akkaStream ++
      akkaHttpTestkit % "test" ++
      akkaHttp ++
      akkaHttpSprayJson ++
      scalactic ++
      intelliJAnnotations % "compile" ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val `http-server` = project.dependsOn(`http-client`, common, data)
  .configs(ForkedTest).settings(forkedSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      sprayJson ++
      akkaHttpTestkit % "test" ++
      akkaHttp ++
      akkaHttpSprayJson ++
      akkaActor ++
      akkaSlf4j ++
      scalactic ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val `engine-job-api` = project.dependsOn(common)
  .configs(ForkedTest).settings(forkedSettings)
  .settings(commonSettings)
  .settings(
    description := "JobScheduler Java Job API",
    crossPaths := false)  // No Scala binary "_2.11" version in artifact name
  .settings {
    import Dependencies._
    libraryDependencies ++=
      guava ++
      javaxAnnotations % "compile" ++
      groovy % "test" ++
      apacheCommonsBeanutils % "test" ++
      reflections % "test" ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val minicom = project.dependsOn(common, `engine-job-api`)
  .configs(ForkedTest).settings(forkedSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      guava ++
      scalaXml ++
      javaxAnnotations % "compile" ++
      intelliJAnnotations % "compile" ++
      mockito % "test" ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val tunnel = project.dependsOn(`tunnel-data`, `http-server`, common, data)
  .configs(ForkedTest).settings(forkedSettings)
  .settings(commonSettings)
  .settings(description := "HTTP TCP Tunnel for JobScheduler API RPC")
  .settings {
    import Dependencies._
    libraryDependencies ++=
      akkaHttp ++
      akkaHttpSprayJson ++
      akkaStream ++
      akkaActor ++
      akkaAgent ++
      akkaSlf4j ++
      sprayJson ++
      scalactic ++
      intelliJAnnotations % "compile" ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val `tunnel-data` = project.dependsOn(common, data, `http-server`/*HeartbeatView is here*/)
  .configs(ForkedTest).settings(forkedSettings)
  .settings(commonSettings)
  .settings(description := "HTTP TCP Tunnel for JobScheduler API RPC - value classes")
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val taskserver = project
  .dependsOn(
    `taskserver-moduleapi`,
    `taskserver-dotnet`,
    `tunnel-data`,
    minicom,
    `agent-data`,
    common,
    data)
  .configs(ForkedTest).settings(forkedSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaXml ++
      akkaActor ++
      akkaSlf4j ++
      scalactic ++
      guava ++
      mockito % "test" ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val `taskserver-moduleapi` = project.dependsOn(minicom, common)
  .configs(ForkedTest).settings(forkedSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val `taskserver-dotnet` = project.dependsOn(`taskserver-moduleapi`, `engine-job-api`, common)
  .configs(ForkedTest).settings(forkedSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      javaxAnnotations % "compile" ++
      "net.sf.jni4net" % "jni4net.j" % "0.8.8.0" ++
      "net.sf.jni4net" % "jni4net-bin" % "0.8.8.0" % "compile" ++
      "com.sos-berlin" % "jni4net.n-sos" % "0.8.8.0" % "compile" ++
      mockito % "test" ++
      scalaTest % "test" ++
      log4j % "test"
  }
  .settings(
    sourceGenerators in Compile += Def.task {
      def extractProxygen() = IO.unzip(
        from = (update in Compile).value.select(artifactFilter()).filter { _.name contains "jni4net-bin-0.8.8.0.jar" }.head,
        toDirectory = (target in Compile).value / "jni4net",
        filter = { name: String ⇒ name.startsWith("bin/proxygen.exe") || name.startsWith("lib/") })

      def extractDll() = IO.unzip(
        from = (update in Compile).value.select(artifactFilter()).filter { _.name contains "jni4net.n-sos-0.8.8.0.jar" }.head,
        toDirectory = (target in Compile).value / "jni4net_forked")

      if (!isWindows) Nil
      else {
        import java.lang.ProcessBuilder.Redirect.PIPE
        import java.nio.file.Files.createDirectories
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

lazy val tests = project.dependsOn(master, agent, `agent-client`)
  .configs(ForkedTest).settings(forkedSettings)
  .settings(
    commonSettings,
    skip in publish := true,
    description := "JobScheduler Tests")
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaTest % "test" ++
      akkaHttpTestkit % "test" ++  // For IntelliJ
      mockito % "test" ++
      log4j % "test"
  }

lazy val ForkedTest = config("ForkedTest") extend Test
lazy val forkedSettings = inConfig(ForkedTest)(Defaults.testTasks) ++ List(
  testOptions in ForkedTest := Seq(Tests.Filter(isIT)),
  fork in ForkedTest := true,
  testOptions in Test := Seq(Tests.Filter(name ⇒ !isIT(name))))

def isIT(name: String): Boolean = name endsWith "IT"

def isTestJar(name: String) = // How to automatically determine/exclude test dependencies ???
  name.startsWith("com.typesafe.akka.akka-testkit_") ||
  name.startsWith("com.typesafe.akka.akka-http-testkit_") ||
  name.startsWith("org.scalatest.scalatest_") ||
  name.startsWith("org.mockito.") ||
  name.startsWith("org.hamcrest.")
