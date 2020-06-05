/**
  * Install sbt from http://www.scala-sbt.org/.
  * Not needed for standard (JVM-only) production build:
  *   Install Node.js from https://nodejs.org/.
  *   If you don't start sbt with "./sbt-batch": Run "npm install jsdom" in this or a parent directory.
  *   This command creates a directory "node_modules" and a file "package-lock.json".
  *
  * Recommended usage for CI server:
  *   sbt clean-publish
  *
  * To build only, without publishing:
  *   sbt clean-build
  *
  * To build and publish to a repository use
  *   sbt -DpublishRepository.credentialsFile=... -DpublishRepository.name=... -DpublishRepository.uri=... clean-publish
  *   (publishRepository.name defaults to publishRepository.uri)
  *
  *   Under Windows, if system properties are not accepted, set a environment variable:
  *   set SBT_OPTS=-DpublishRepository.credentialsFile=... -DpublishRepository.name=... -DpublishRepository.uri=...
  *   sbt clean-publish
  *
  * sbt allows to preset these command line options in the environment variable SBT_OPTS.
  */
import BuildUtils._
import java.nio.file.Paths
import sbt.Keys.testOptions
import sbt.file
// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.crossProject

val rootDirectory = Paths.get(".").toAbsolutePath

val publishRepositoryCredentialsFile = sys.props.get("publishRepository.credentialsFile").map(o => new File(o))
val publishRepositoryName            = sys.props.get("publishRepository.name")
val publishRepositoryUri             = sys.props.get("publishRepository.uri")
// BuildUtils reads more properties.
val isForDevelopment                 = sys.props contains "dev"

addCommandAlias("clean-all"      , "; clean; js7JS/clean; js7-tester/clean")
addCommandAlias("clean-publish"  , "; clean-all; build; publish-all")
addCommandAlias("clean-build"    , "; clean-all; build")
addCommandAlias("clean-build-only", "; clean-all; build-only")
addCommandAlias("clean-pack"     , "; clean-all; compile-only; pack")
addCommandAlias("build"          , "; compile-all; test-all; pack")
addCommandAlias("build-only"     , "; compile-only; pack")
addCommandAlias("compile-all"    , "; Test/compile; ForkedTest/compile; ExclusiveTest/compile")
addCommandAlias("compile-only"   , "; compile")
addCommandAlias("test-all",
  if (testParallelization > 1)
    "; StandardTest:test" +
    "; ForkedTest:test" +
    "; set Global/concurrentRestrictions += Tags.exclusive(Tags.Test)" +  //Tags.limit(Tags.Test, max = 1)" +  // Slow: Tags.limitAll(1)
    "; ExclusiveTest:test"
  else
    "; test" +
    "; ForkedTest:test")
addCommandAlias("pack"           , "universal:packageZipTarball")
addCommandAlias("publish-all"    , "universal:publish")  // Publishes artifacts too
addCommandAlias("publish-install", "; install/universal:publish; install-docker:universal:publish")
addCommandAlias("TestMasterAgent", "js7-tests/runMain js7.tests.TestMasterAgent -agents=2 -nodes-per-agent=3 -tasks=3 -job-duration=1.5s -period=10.s")
addCommandAlias("quickPublishLocal", "; compile; publishLocal; project js7JS; compile; publishLocal")
//scalafixDependencies in ThisBuild += "org.scala-lang.modules" %% "scala-collection-migrations" % "2.1.4"
//addCompilerPlugin(scalafixSemanticdb)
//ThisBuild / scalacOptions ++= Seq("-P:semanticdb:synthetics:on", "-Yrangepos"/*required by SemanticDB compiler plugin*/)
val enableWarnings = Seq(
  "-Wunused:imports",
  "-Wunused:privates",
  "-Wunused:locals",
  "-Wunused:implicits",
  "-Xlint:infer-any",
  "-Xlint:doc-detached",
  "-Xlint:private-shadow",
  //"-Xlint:type-parameter-shadow",
  "-Xlint:poly-implicit-overload",
  "-Xlint:constant",
  "-Xlint:implicit-not-found",
  "-Xlint:eta-zero")

//scalafixDependencies in ThisBuild += "org.scalatest" %% "autofix" % "3.1.0.0"
//addCompilerPlugin(scalafixSemanticdb) // enable SemanticDB
val jdkVersion = "1.8"

ThisBuild / scalacOptions ++= Seq(
  "-Ymacro-annotations",
  "-unchecked",
  "-deprecation",
  "-feature")

// http://www.scalatest.org/user_guide/using_scalatest_with_sbt
val scalaTestArguments = Tests.Argument(TestFrameworks.ScalaTest,
  (if (testParallelization > 1) "-oNCLPQF" :: Nil else Nil) ::: List("-W", "30", "30"): _*)

val _dummy_ = {
  sys.props("TEST") = "true"
}

val publishSettings = Seq(
  publishArtifact in (Compile, packageDoc) := false,
  credentials ++= publishRepositoryCredentialsFile.map(o => Credentials(o)),
  publishTo := publishRepositoryUri.map(uri => publishRepositoryName getOrElse uri at uri))

maintainer := "Joacim Zschimmer <jogit@zschimmer.com>"
val commonSettings = Seq(
  organization := "sh.js7.engine",
  organizationName := "SOS Berlin",
  organizationHomepage := Some(url("https://js7.sh")),
  pomExtra :=
    <developers>
      <developer>
          <name>Joacim Zschimmer</name>
          <organization>{organizationName.value}</organization>
          <organizationUrl>{organizationHomepage.value.get}</organizationUrl>
      </developer>
    </developers>,
  scalaVersion := Dependencies.scalaVersion,
  javacOptions in Compile ++= Seq("-encoding", "UTF-8", "-source", jdkVersion),  // This is for javadoc, too
  javacOptions in (Compile, compile) ++= Seq("-target", jdkVersion, "-deprecation", "-Xlint:all", "-Xlint:-serial", "-Xdiags:verbose"),
  dependencyOverrides ++= {
    if (sys.props.contains("evictionWarnings"))
      Nil
    else {
      import Dependencies._
      cats ++
        ("org.typelevel" %% "cats-core" % catsVersion) ++
        ("org.typelevel" %% "cats-effect" % catsEffectVersion) ++
        circe ++
        //("org.sangria-graphql" %% "sangria-marshalling-api" % sangriaVersion) ++  // sangria-circe uses an older version
        scalaXml ++ guava ++ slf4j
      }
  },
  sources in (Compile, doc) := Nil, // No ScalaDoc
  test in publishM2 := {},
  // Publish
  publishArtifact in (Compile, packageDoc) := false,
  credentials += publishRepositoryCredentialsFile.map(o => Credentials(o)),
  publishTo := publishRepositoryUri.map(uri => publishRepositoryName getOrElse uri at uri))

useJGit
git.uncommittedSignifier := Some("UNCOMMITTED")

val universalPluginSettings = Seq(
  universalArchiveOptions in (Universal, packageZipTarball) :=
    (universalArchiveOptions in (Universal, packageZipTarball)).value)

resolvers += Resolver.mavenLocal

lazy val js7 = (project in file("."))
  .aggregate(
    `js7-agent`,
    `js7-base`.jvm,
    `js7-common`,
    `js7-common-http`.jvm,
    `js7-core`,
    `js7-data`.jvm,
    `js7-docker`,
    `js7-install`,
    `js7-master`,
    `js7-master-client`.jvm,
    `js7-master-data`.jvm,
    `js7-agent-client`,
    `js7-agent-data`,
    `js7-taskserver`,
    `js7-provider`,
    `js7-proxy`.jvm,
    `js7-tests`,
    `build-info`)
  .settings(skip in publish := true)

lazy val js7JS = (project in file("target/project-js7JS"))
  .aggregate(
    `js7-base`.js,
    `js7-common-http`.js,
    `js7-data`.js,
    `js7-master-client`.js,
    `js7-master-data`.js,
    `js7-proxy`.js)
  .settings(skip in publish := true)

lazy val all = (project in file("target/project-all"))  // Not the default project
  .aggregate(js7, js7JS)

lazy val `js7-install` = project
  .dependsOn(`js7-master`, `js7-provider`, `js7-agent`)
  .settings(commonSettings)
  .enablePlugins(JavaAppPackaging, UniversalDeployPlugin)
  .settings {
    import Dependencies._
    libraryDependencies ++= log4j ++ lmaxDisruptor
  }
  .settings(
    //skip in publish := true,  // We publish only .tgz and .zip generated by sbt-native-packager / UniversalDeployPlugin
    universalPluginSettings,
    topLevelDirectory in Universal := Some(s"js7-${version.value}"),
    mappings in Universal :=
      (((mappings in Universal).value filter { case (_, path) => (path startsWith "lib/") && !doNotInstallJar(path stripPrefix "lib/") }) ++
        NativePackagerHelper.contentOf((`js7-master` / Compile / classDirectory).value / "js7/master/installation") ++
        NativePackagerHelper.contentOf((`js7-provider` / Compile / classDirectory).value / "js7/provider/installation") ++
        NativePackagerHelper.contentOf((`js7-agent`  / Compile / classDirectory).value / "js7/agent/installation") ++
        NativePackagerHelper.contentOf((`js7-core`   / Compile / classDirectory).value / "js7/core/installation")
      ).toVector.sortBy(_._2))

lazy val `js7-docker` = project
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++= log4j ++ Dependencies.lmaxDisruptor
  }
  .enablePlugins(JavaAppPackaging, UniversalDeployPlugin)
  .settings(
    universalPluginSettings,
    topLevelDirectory in Universal := None,
    mappings in Universal :=
      NativePackagerHelper.contentOf(baseDirectory.value / "src/main/resources/js7/install/docker/")
        .map { case (file, dest) => file -> ("build/" + dest) })

lazy val `js7-tester` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .settings(commonSettings)
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      "io.circe" %%% "circe-core" % circeVersion ++
      "io.circe" %%% "circe-parser" % circeVersion ++
      "io.circe" %%% "circe-generic" % circeVersion ++
      "org.scalatest" %%% "scalatest" % scalaTestVersion /*++
      "org.scalatest" %%% "scalatest-freespec" % scalaTestVersion*/
  }

lazy val `js7-base` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .dependsOn(`js7-tester` % "test")
  .settings(commonSettings)
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      "org.typelevel" %%% "cats-core" % catsVersion ++
      "org.typelevel" %%% "cats-laws" % catsVersion % "test" ++
      "org.typelevel" %%% "discipline-core" % disciplineVersion % "test" ++
      "org.typelevel" %%% "discipline-scalatest" % disciplineVersion % "test" ++
      "io.circe" %%% "circe-core" % circeVersion ++
      "io.circe" %%% "circe-parser" % circeVersion ++
      "io.circe" %%% "circe-generic" % circeVersion ++
      "io.circe" %%% "circe-generic-extras" % circeVersion ++
      "io.monix" %%% "monix-eval" % monixVersion ++
      "io.monix" %%% "monix-reactive" % monixVersion ++
      "org.scodec" %%% "scodec-bits" % "1.1.14" ++
      "org.scodec" %%% "scodec-cats" % "1.0.0" ++
      "com.lihaoyi" %%% "sourcecode" % "0.2.1" ++
      "com.outr" %%% "scribe" % scribeVersion ++
      "org.scalactic" %%% "scalactic" % scalaTestVersion ++
      findbugs % "compile" ++
      intelliJAnnotations % "compile" ++
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test" ++
    //"org.scalatest" %%% "scalatest-freespec" % scalaTestVersion % "test" ++
      "org.scalatestplus" %%% "scalacheck-1-14" % scalaTestCheckVersion % "test" ++
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % "test"
  }
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoPackage := "js7.base",
    buildInfoKeys := buildInfoMap.value.map(BuildInfoKey.constant(_)).toSeq)

/** build-info provides version info in a Scala-free jar. */
lazy val `build-info` = (project in file("target/project-build-info"))
  .settings(commonSettings)
  .settings(
    crossPaths := false,
    autoScalaLibrary := false)
  .settings(
    Compile / resourceGenerators += Def.task {
      val file = (Compile / resourceManaged).value / "js7/build-info/build-info.properties"
      IO.write(
        file,
        buildInfoMap.value
          .mapValues {
            case v: Option[_] => v.fold("")(_.toString)
            case v => v.toString
          }
          .map { case (k, v) => s"$k=${v.trim.replace('\n', '⏎')}\n"}
          .mkString)
      Seq(file)
    }.taskValue)

lazy val `js7-data` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .dependsOn(`js7-base`, `js7-tester` % "test")
  .settings(commonSettings)
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      "com.lihaoyi" %%% "fastparse" % fastparseVersion ++
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test" ++
    //"org.scalatest" %%% "scalatest-freespec" % scalaTestVersion % "test" ++
      "org.typelevel" %%% "cats-laws" % catsVersion % "test" ++
      "org.typelevel" %%% "discipline-core" % disciplineVersion % "test" ++
      "org.typelevel" %%% "discipline-scalatest" % disciplineVersion % "test" ++
      "com.github.mpilquist" %% "simulacrum" % simulacrumVersion
  }

lazy val `js7-common` = project.dependsOn(`js7-base`.jvm, `js7-data`.jvm, `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      "io.monix" %%% "monix-eval" % monixVersion ++
      "io.monix" %%% "monix-reactive" % monixVersion ++
      scalaXml ++
      scalaLogging ++
      javaxInject ++
      guice ++
      typesafeConfig ++
      akkaHttpTestkit % "test" ++
      akkaHttp ++
      akkaActor ++
      akkaSlf4j ++
      guava ++
      snakeYaml ++
      findbugs % "compile" ++
      scalaTest % "test" ++
      mockito % "test" ++
      log4j % "test"
    }
  .enablePlugins(GitVersioning)

lazy val `js7-common-http` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .dependsOn(`js7-data`, `js7-base`, `js7-tester` % "test")
  .jvmConfigure(_.dependsOn(`js7-common`))
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++= scalaTest % "test"
    libraryDependencies += "io.monix" %%% "monix-eval" % monixVersion
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
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % Dependencies.scalaJsDomVersion)

lazy val `js7-master` = project.dependsOn(`js7-master-data`.jvm, `js7-master-client`.jvm, `js7-core`, `js7-common`, `js7-agent-client`, `js7-tester`.jvm % "test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  .settings(
    mappings in (Compile, packageDoc) := Seq.empty)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      "org.typelevel" %% "cats-effect" % catsEffectVersion ++
      "org.sangria-graphql" %% "sangria" % sangriaVersion ++
      "org.sangria-graphql" %% "sangria-circe" % sangriaCirceVersion ++
      scalaTest % "test" ++
      akkaHttpTestkit % "test" ++
      log4j % "test"
  }

lazy val `js7-provider` = project.dependsOn(`js7-master-data`.jvm, `js7-master`, `js7-master-client`.jvm, `js7-core`, `js7-common`, `js7-tester`.jvm % "test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  .settings(
    mappings in (Compile, packageDoc) := Seq.empty)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val `js7-proxy` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .dependsOn(`js7-master-client`, `js7-tester` % "test")
  //.jvmConfigure(_.dependsOn(`js7-common`))
  .settings(commonSettings)
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(
    libraryDependencies ++= {
      import Dependencies._
      "io.vavr" % "vavr" % "0.10.3" ++
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test" /*++
      "org.scalatest" %%% "scalatest-freespec" % scalaTestVersion % "test"*/
    })
  .jvmSettings(
    libraryDependencies ++= {
      import Dependencies._
      akkaHttp ++
      "org.scala-lang.modules" %% "scala-java8-compat" % "0.9.1" ++
      hamcrest % "test" ++
      log4j % "test"
    })

lazy val `js7-master-data` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .dependsOn(`js7-data`, `js7-tester` % "test")
  .settings(commonSettings)
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(
    libraryDependencies += {
      import Dependencies._
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test" /*++
      "org.scalatest" %%% "scalatest-freespec" % scalaTestVersion % "test"*/
    })

lazy val `js7-master-client` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .dependsOn(`js7-master-data`, `js7-common-http`, `js7-tester` % "test")
  .jvmConfigure(_.dependsOn(`js7-common`))
  .settings(commonSettings)
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(
    libraryDependencies += {
      import Dependencies._
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test" /*++
      "org.scalatest" %%% "scalatest-freespec" % scalaTestVersion % "test"*/
    })
  .jvmSettings(
    libraryDependencies ++= {
      import Dependencies._
      akkaHttp ++
      log4j % "test"
    })

lazy val `js7-core` = project.dependsOn(`js7-common`, `js7-tester`.jvm % "test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      shapeless ++
      bouncyCastle ++
      diffx ++
      akkaHttpTestkit % "test" ++
      scalaTest % "test" ++
      scalaCheck % "test" ++
      log4j % "test"
  }
  .settings(
    resourceGenerators in Compile += Def.task {
      val versionFile = (resourceManaged in Compile).value / "js7/core/installation/VERSION"
      IO.write(versionFile, BuildUtils.longVersion.value + "\n")
      Seq(versionFile)
    }.taskValue)

lazy val `js7-agent` = project.dependsOn(`js7-agent-data`, `js7-core`, `js7-common`, `js7-data`.jvm, `js7-taskserver`, `js7-agent-client` % "test", `js7-tester`.jvm % "test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaXml ++
      guava ++
      findbugs % "compile" ++
      akkaActor ++
      akkaStream ++
      akkaSlf4j ++
      akkaHttpTestkit % "test" ++
      akkaHttp ++
      intelliJAnnotations % "compile" ++
      scalactic ++
      guice ++
      mockito % "test" ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val `js7-agent-client` = project.dependsOn(`js7-data`.jvm, `js7-common-http`.jvm, `js7-common`, `js7-agent-data`, `js7-tester`.jvm % "test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  .settings(description := "JS7 Agent - Client")
  .settings {
    import Dependencies._
    libraryDependencies ++=
      guice ++
      "io.monix" %% "monix-reactive" % monixVersion ++
      akkaActor ++
      akkaHttp ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val `js7-agent-data` = project.dependsOn(`js7-common`, `js7-data`.jvm, `js7-tester`.jvm % "test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(commonSettings)
  .settings(description := "JS7 Agent - Value Classes")
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaXml ++
      guava ++
      findbugs % "compile" ++
      intelliJAnnotations % "compile" ++
      scalaTest % "test" ++
      log4j % "test"
  }

lazy val `js7-taskserver` = project
  .dependsOn(
    `js7-agent-data`,
    `js7-common`,
    `js7-data`.jvm,
    `js7-tester`.jvm % "test")
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

lazy val `js7-tests` = project.dependsOn(`js7-master`, `js7-agent`, `js7-proxy`.jvm, `js7-agent-client`, `js7-provider`, `js7-tester`.jvm % "test", `js7-docker` % "test")
  .configs(StandardTest, ExclusiveTest, ForkedTest).settings(testSettings)
  .settings(
    commonSettings,
    skip in publish := true,
    description := "JS7 Tests")
  .settings {
    import Dependencies._
    libraryDependencies ++=
      akkaHttpTestkit % "test" ++  // For IntelliJ IDEA 2018.2
      scalaTest % "test" ++
      mockito % "test" ++
      hamcrest % "test" ++
      log4j % "test"
  }

Global / concurrentRestrictions += Tags.limit(Tags.Test, max = testParallelization)

lazy val StandardTest  = config("StandardTest" ) extend Test
lazy val ExclusiveTest = config("ExclusiveTest") extend Test
lazy val ForkedTest    = config("ForkedTest"   ) extend Test
lazy val testSettings =
  inConfig(StandardTest )(Defaults.testTasks) ++
  inConfig(ExclusiveTest)(Defaults.testTasks) ++
  inConfig(ForkedTest   )(Defaults.testTasks) ++
  Seq(
    Test          / testOptions := Seq(scalaTestArguments, Tests.Filter(name => !isForkedTest(name))),  // Exclude ForkedTest from sbt command "test" because ForkedTest will fail when not forked
    StandardTest  / testOptions := Seq(scalaTestArguments, Tests.Filter(isStandardTest)),
    ExclusiveTest / testOptions := Seq(scalaTestArguments, Tests.Filter(isExclusiveTest)),
    ForkedTest / fork := true,
    ForkedTest / testOptions := Seq(scalaTestArguments, Tests.Filter(isForkedTest)),
    ForkedTest / javaOptions ++= Seq("-Xmx100m", "-Xms20m"),
    ForkedTest / javaOptions += s"-Dlog4j.configurationFile=$rootDirectory/project/log4j2.xml",  // Does not work !!! "ERROR StatusLogger No Log4j 2 configuration file found"
    ForkedTest / testForkedParallel := testParallelization > 1,
    Test          / logBuffered := false,  // Recommended for ScalaTest
    StandardTest  / logBuffered := false,
    ExclusiveTest / logBuffered := false,
    ForkedTest    / logBuffered := false)

def isStandardTest(name: String): Boolean = !isExclusiveTest(name) && !isForkedTest(name)
def isExclusiveTest(name: String): Boolean = name endsWith "ExclusiveTest"
def isForkedTest(name: String): Boolean = name endsWith "ForkedTest"

def doNotInstallJar(path: String) = false
