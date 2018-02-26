/**
  * Install sbt from http://www.scala-sbt.org/.
  * Install Node.js from https://nodejs.org/.
  *   Run "npm install jsdom" in a parent directory.
  *   This command creates a directory "node_modules" and a file "package-lock.json".
  *
  * Recommended usage for CI server:
  *   sbt clean-publish
  *
  * To build only, without publishing:
  *   sbt clean-build
  *
  * To build quickly, without running tests again:
  *   sbt "; clean; build-quickly"
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
import java.nio.file.Paths
import sbt.Keys.testOptions
import sbt.librarymanagement.DependencyFilter.artifactFilter
import sbt.{CrossVersion, Def}

val _dummy_ = BuildUtils.initializeLogger()

val publishRepositoryCredentialsFile = sys.props.get("publishRepository.credentialsFile") map (o ⇒ new File(o))
val publishRepositoryName            = sys.props.get("publishRepository.name")
val publishRepositoryUri             = sys.props.get("publishRepository.uri")
val testParallel                     = sys.props contains "test.parallel"
val isForDevelopment                 = sys.props contains "dev"

// Under Windows, compile engine-job-api first, to allow taskserver-dotnet accessing the class files of engine-job-api.
addCommandAlias("clean-publish"  , "; clean ;build; publish-all")
addCommandAlias("clean-build"    , "; clean; build")
addCommandAlias("build"          , "; compile-all; test-all; pack")
addCommandAlias("build-quickly"  , "; compile-all; pack")
addCommandAlias("compile-all"    , "; engine-job-api/compile; compile")
addCommandAlias("test-all",
  "; test:compile" +
  "; ForkedTest:compile" + (
    if (testParallel)
      "; StandardTest:test" +
      "; ForkedTest:test" +
      "; set Global/concurrentRestrictions += Tags.exclusive(Tags.Test)" +  //Tags.limit(Tags.Test, max = 1)" +  // Slow: Tags.limitAll(1)
      "; ExclusiveTest:test"
    else
      "; test" +
      "; ForkedTest:test"))
addCommandAlias("pack"           , "universal:packageZipTarball")
addCommandAlias("publish-all"    , "universal:publish")  // Publishes artifacts too
addCommandAlias("publish-install", "; install/universal:publish; install-docker:universal:publish")
addCommandAlias("TestMasterAgent", "master/runMain com.sos.jobscheduler.master.tests.TestMasterAgent -agents=2 -nodes-per-agent=3 -tasks=3 -job-duration=1.5s -period=10.s")

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")
val scalaTestArguments = Tests.Argument(TestFrameworks.ScalaTest, "-oFCLOPQD")  // http://www.scalatest.org/user_guide/using_scalatest_with_sbt

val publishSettings = Seq(
  publishArtifact in (Compile, packageDoc) := false,
  credentials ++= publishRepositoryCredentialsFile map (o ⇒ Credentials(o)),
  publishTo := publishRepositoryUri map (uri ⇒ publishRepositoryName getOrElse uri at uri))

val commonSettings = Seq(
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
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  javacOptions in Compile ++= Seq("-encoding", "UTF-8", "-source", "1.8"),  // This is for javadoc, too
  javacOptions in (Compile, compile) ++= Seq("-target", "1.8", "-deprecation", "-Xlint:all", "-Xlint:-serial"),
  dependencyOverrides += Dependencies.guava, // Our Guava version should be okay
  dependencyOverrides ++= Seq(Dependencies.akkaActor, Dependencies.akkaStream), // akka-http requests a slightly older version of Akka
  sources in (Compile, doc) := Nil, // No ScalaDoc
  test in publishM2 := {},
  // Publish
  publishArtifact in (Compile, packageDoc) := false,
  credentials += publishRepositoryCredentialsFile map (o ⇒ Credentials(o)),
  publishTo := publishRepositoryUri map (uri ⇒ publishRepositoryName getOrElse uri at uri))

val universalPluginSettings = Seq(
  universalArchiveOptions in (Universal, packageZipTarball) :=
    Seq("--force-local") .filter { _ ⇒ !isMac } ++
      (universalArchiveOptions in (Universal, packageZipTarball)).value)  // Under cygwin, tar shall not interpret C:

resolvers += Resolver.mavenLocal

lazy val jobscheduler = (project in file("."))
  .aggregate(
    agent,
    base.jvm,
    common,
    `common-http`.jvm,
    core,
    data.jvm,
    `jobscheduler-docker`,
    `jobscheduler-install`,
    master,
    `master-client`.jvm,
    `master-data`.jvm,
    `master-client`.jvm,
    `agent-client`,
    `agent-data`,
    `agent-test`,
    `agent-tests`,
    `engine-job-api`,
    minicom,
    taskserver,
    `taskserver-dotnet`,
    `taskserver-moduleapi`,
    tests,
    tunnel)
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
        recursiveFileMapping(baseDirectory.value / "../core/src/main/resources/com/sos/jobscheduler/core/installation"))

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

lazy val tester = crossProject
  .settings(commonSettings)
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      "io.circe" %%% "circe-core" % circeVersion ++
      "io.circe" %%% "circe-parser" % circeVersion ++
      "io.circe" %%% "circe-generic" % circeVersion ++
      "org.scalatest" %%% "scalatest" % scalaTestVersion
  }
lazy val testerJVM = tester.jvm
lazy val testerJs = tester.js

lazy val base = crossProject
  .dependsOn(tester % "compile->test")
  .settings(commonSettings)
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      "org.typelevel" %%% "cats-core" % catsVersion ++
      "org.typelevel" %%% "cats-laws" % catsVersion % "test" ++
      "org.typelevel" %%% "discipline" % disciplineVersion % "test" ++
      "io.circe" %%% "circe-core" % circeVersion ++
      "io.circe" %%% "circe-parser" % circeVersion ++
      "io.circe" %%% "circe-generic" % circeVersion ++
      "io.circe" %%% "circe-generic-extras" % circeVersion ++
      "io.monix" %%% "monix" % monixVersion ++
      javaxAnnotations % "compile" ++
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test"
  }
lazy val baseJVM = base.jvm
lazy val baseJs = base.js

lazy val data = crossProject
  .dependsOn(base, tester % "compile->test")
  .settings(commonSettings)
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test" ++
      "org.typelevel" %%% "cats-laws" % catsVersion % "test" ++
      "org.typelevel" %%% "discipline" % disciplineVersion % "test"
  }
lazy val dataJVM = data.jvm
lazy val dataJs = data.js

lazy val common = project.dependsOn(`common-http`.jvm, base.jvm, data.jvm, tester.jvm % "compile->test")
  .settings(commonSettings)
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
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
      akkaActor ++
      akkaSlf4j ++
      scalaTags ++
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
    buildInfoKeys := List[BuildInfoKey](
      "buildVersion" → VersionFormatter.buildVersion(
        version = version.value,
        versionCommitHash = git.gitHeadCommit.value,
        branch = git.gitCurrentBranch.value),
      "version" → version.value,
      BuildInfoKey.action("buildId")(newBuildId)),
    buildInfoPackage := "com.sos.jobscheduler.common")

lazy val `common-http` = crossProject
  .dependsOn(base, tester % "compile->test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies +=
      scalaTest % "test"
  }
  .jvmSettings {
    import Dependencies._
    libraryDependencies ++=
      akkaHttp ++
      snakeYaml ++
      scalaLogging ++
      log4j % "test"
  }
  .jsSettings(
    //scalacOptions += "-P:scalajs:sjsDefinedByDefault",  // Scala.js 0.6 behaves as Scala.js 1.0, https://www.scala-js.org/doc/interoperability/sjs-defined-js-classes.html
    //jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(),  // For tests. Requires: npm install jsdom
    //scalaJSStage in Global := (if (isForDevelopment) FastOptStage else FullOptStage),
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1")

val masterGuiPath = s"com/sos/jobscheduler/master/gui/frontend/gui"
val masterGuiJs = "master-gui.js"

lazy val master = project.dependsOn(`master-data`.jvm, `master-client`.jvm, core, common, `agent-client`, tester.jvm % "compile->test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  // Provide master-gui JavaScript code as resources placed in master-gui package
  .settings(
    resources in Compile ++= Seq(
      new File((scalaJSLinkedFile in Compile in `master-gui`).value.path),            // master-gui-fastopt.js or master-gui-opt.js
      new File((scalaJSLinkedFile in Compile in `master-gui`).value.path + ".map"),   // master-gui-...opt.js.map
      (packageMinifiedJSDependencies in Compile in `master-gui`).value),              // master-gui-...opt-jsdeps.min.js
    mappings in (Compile, packageBin) :=
      (mappings in (Compile, packageBin)).value map { case (file, path) ⇒
        val generatedJsName = Paths.get((scalaJSLinkedFile in Compile in `master-gui`).value.path).toFile.getName
        val jsMapName = s"$generatedJsName.map"  // Keep original .js.map name
        val minJsDepName = (packageMinifiedJSDependencies in Compile in `master-gui`).value.getName
        path match {
          case `generatedJsName` ⇒
            println(s"$generatedJsName -> $masterGuiPath/$masterGuiJs")  // Show ...-fastopt.js or ...-opt.js
            (file, s"$masterGuiPath/$masterGuiJs")  // Move to package and use the same name for fastOptJS and fullOptJS generated JavaScript file
          case `jsMapName` | `minJsDepName`  ⇒
            (file, s"$masterGuiPath/$path")
          case _ ⇒
            (file, path)
        }
      },
    mappings in (Compile, packageBin) := {  // All assets from master-gui/$masterGuiPath: index.html, .css, .ico etc.
      val m = recursiveFileMapping((classDirectory in Compile in `master-gui`).value / masterGuiPath, to = masterGuiPath).toMap
      ((mappings in (Compile, packageBin)).value filterNot { case (file, _) ⇒ m contains file }) ++ m
    },
    mappings in (Compile, packageDoc) := Seq())
  .settings {
    import Dependencies._
    libraryDependencies ++=
      webjars.materialIcons ++
      webjars.materializeCss ++
      scalaTest % "test" ++
      akkaHttpTestkit % "test" ++
      akkaHttp/*force version?*/ % "test" ++
      log4j % "test"
  }

lazy val `master-data` = crossProject
  .dependsOn(data, tester % "compile->test")
  .settings(commonSettings)
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(
    libraryDependencies ++= {
      import Dependencies._
      Seq(
        "org.scalatest" %%% "scalatest" % scalaTestVersion % "test")
    })
lazy val `master-dataJVM` = `master-data`.jvm
lazy val `master-dataJs` = `master-data`.js

lazy val `master-client` = crossProject
  .dependsOn(`master-data`, `common-http`, tester % "compile->test")
  .jvmConfigure(_.dependsOn(common))
  .settings(commonSettings)
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(
    libraryDependencies ++= {
      import Dependencies._
      Seq(
        "org.scalatest" %%% "scalatest" % scalaTestVersion % "test")
    })
  .jvmSettings(
    libraryDependencies ++= {
      import Dependencies._
      akkaHttp ++
      log4j % "test"
    })
  .jsSettings(
    scalacOptions += "-P:scalajs:sjsDefinedByDefault",  // Scala.js 0.6 behaves as Scala.js 1.0, https://www.scala-js.org/doc/interoperability/sjs-defined-js-classes.html
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(),  // For tests. Requires: npm install jsdom
    scalaJSStage in Global := (if (isForDevelopment) FastOptStage else FullOptStage),
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1")

lazy val `master-clientJVM` = `master-client`.jvm
lazy val `master-clientJs` = `master-client`.js

lazy val `master-gui` = project
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(data.js, `master-data`.js, `master-client`.js)
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(
    commonSettings,
    scalacOptions += "-P:scalajs:sjsDefinedByDefault",  // Scala.js 0.6 behaves as Scala.js 1.0, https://www.scala-js.org/doc/interoperability/sjs-defined-js-classes.html
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(),  // For tests. Requires: npm install jsdom
    scalaJSUseMainModuleInitializer := true,
    scalaJSStage in Global := (if (isForDevelopment) FastOptStage else FullOptStage),
    libraryDependencies ++= {
      import Dependencies._
      Seq(
        "com.github.mpilquist" %% "simulacrum" % simulacrumVersion,
        "org.scala-js" %%% "scalajs-dom" % "0.9.4",
        "be.doeraene" %%% "scalajs-jquery" % "0.9.2",
        "com.github.japgolly.scalajs-react" %%% "core" % "1.1.1",
        "com.github.japgolly.scalajs-react" %%% "extra" % "1.1.1",
        "org.scalatest" %%% "scalatest" % scalaTestVersion % "test")
    },
    jsDependencies ++= Seq(
      "org.webjars.bower" % "jquery" % "3.2.1"           / "dist/jquery.js" minified "dist/jquery.min.js",
      "org.webjars"       % "materializecss" % "0.100.2" / "materialize.js" minified "materialize.min.js" dependsOn "dist/jquery.js",
      "org.webjars.bower" % "react" % "15.6.1"     / "react-with-addons.js" minified "react-with-addons.min.js" commonJSName "React",
      "org.webjars.bower" % "react" % "15.6.1"     / "react-dom.js"         minified "react-dom.min.js"         commonJSName "ReactDOM"       dependsOn "react-with-addons.js",
      "org.webjars.bower" % "react" % "15.6.1"     / "react-dom-server.js"  minified "react-dom-server.min.js"  commonJSName "ReactDOMServer" dependsOn "react-dom.js",
      "org.webjars.npm"   % "react-transition-group" % "2.2.1" / "dist/react-transition-group.js" minified "dist/react-transition-group.min.js" dependsOn "react-dom.js"))

lazy val core = project.dependsOn(common, tester.jvm % "compile->test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      fastparse ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val agent = project.dependsOn(`agent-data`, core, common, data.jvm, taskserver, tunnel, tester.jvm % "compile->test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaXml ++
      guava ++
      javaxAnnotations % "compile" ++
      akkaActor ++
      akkaStream ++
      akkaSlf4j ++
      akkaHttpTestkit % "test" ++
      akkaHttp ++
      intelliJAnnotations % "compile" ++
      scalactic ++
      tagging ++
      guice ++
      mockito % "test" ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val `agent-client` = project.dependsOn(data.jvm, `common-http`.jvm, common, `agent-test` % "compile->test", tester.jvm % "compile->test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  .settings(description := "JobScheduler Agent - Client")
  .settings {
    import Dependencies._
    libraryDependencies ++=
      guice ++
      akkaActor ++
      akkaHttp ++
      scalaTest % "test" ++
      log4j % "test"
  }
lazy val `common-httpJVM` = `common-http`.jvm
lazy val `common-httpJs` = `common-http`.js

lazy val `agent-data` = project.dependsOn(common, data.jvm, tester.jvm % "compile->test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  .settings(description := "JobScheduler Agent - Value Classes")
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaXml ++
      guava ++
      javaxAnnotations % "compile" ++
      intelliJAnnotations % "compile" ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val `agent-test` = project.dependsOn(agent, common, tester.jvm % "compile->test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaTest ++
      log4j % "test"
  }

lazy val `agent-tests` = project.dependsOn(`agent` % "test->test", `agent-client` % "test->test", tester.jvm % "compile->test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
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

lazy val `engine-job-api` = project.dependsOn(common, tester.jvm % "compile->test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
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

lazy val minicom = project.dependsOn(common, `engine-job-api`, tester.jvm % "compile->test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
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

lazy val tunnel = project.dependsOn(common, data.jvm, tester.jvm % "compile->test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  .settings(description := "HTTP TCP Tunnel for JobScheduler API RPC")
  .settings {
    import Dependencies._
    libraryDependencies ++=
      akkaHttp ++
      akkaStream ++
      akkaActor ++
      akkaSlf4j ++
      scalactic ++
      intelliJAnnotations % "compile" ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val taskserver = project
  .dependsOn(
    `taskserver-moduleapi`,
    `taskserver-dotnet`,
    minicom,
    `agent-data`,
    common,
    data.jvm,
    tester.jvm % "compile->test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
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

lazy val `taskserver-moduleapi` = project.dependsOn(minicom, common, tester.jvm % "compile->test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val `taskserver-dotnet` = project.dependsOn(`taskserver-moduleapi`, `engine-job-api`, common, tester.jvm % "compile->test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
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
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
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

Global / concurrentRestrictions ++= (if (testParallel) Nil else Seq(Tags.exclusive(Tags.Test)))


lazy val StandardTest  = config("StandardTest" ) extend Test
lazy val ExclusiveTest = config("ExclusiveTest") extend Test
lazy val ForkedTest    = config("ForkedTest"   ) extend Test
lazy val testSettings =
  inConfig(StandardTest )(Defaults.testTasks) ++
  inConfig(ExclusiveTest)(Defaults.testTasks) ++
  inConfig(ForkedTest   )(Defaults.testTasks) ++
  Seq(
    testOptions in Test          := Seq(scalaTestArguments, Tests.Filter(name ⇒ !isForkedTest(name))),  // Exclude ForkedTest from sbt command "test" because ForkedTest will fail when not forked
    testOptions in StandardTest  := Seq(scalaTestArguments, Tests.Filter(isStandardTest)),
    testOptions in ExclusiveTest := Seq(scalaTestArguments, Tests.Filter(isExclusiveTest)),
    testOptions in ForkedTest    := Seq(scalaTestArguments, Tests.Filter(isForkedTest)),
    javaOptions in ForkedTest    ++= Seq("-Xmx100m", "-Xms20m", "-Dlog4j.configurationFile=../project/log4j2.xml"),  // SettingKey for ../project ???
    testForkedParallel in ForkedTest := testParallel,  // Experimental in sbt 0.13.13
    logBuffered in Test          := false,  // Recommended for ScalaTest
    logBuffered in StandardTest  := false,
    logBuffered in ExclusiveTest := false,
    logBuffered in ForkedTest    := false,
    fork in ForkedTest := true)

def isStandardTest(name: String): Boolean = !isExclusiveTest(name) && !isForkedTest(name)
def isExclusiveTest(name: String): Boolean = name endsWith "ExclusiveTest"
def isForkedTest(name: String): Boolean = name endsWith "ForkedTest"

def isTestJar(name: String) = // How to automatically determine/exclude test dependencies ???
  name.startsWith("com.typesafe.akka.akka-testkit_") ||
  name.startsWith("com.typesafe.akka.akka-http-testkit_") ||
  name.startsWith("org.scalatest.scalatest_") ||
  name.startsWith("org.mockito.") ||
  name.startsWith("org.hamcrest.")
