// WARNING: Run tests only in a secure closed environment (like Docker) !!!
// because tests open localhost TCP ports that may allow code injection.
/**
 * Install sbt from https://www.scala-sbt.org/.
 *
 * NOT NEEDED for standard (JVM-only) production build:
 *    Install Node.js from https://nodejs.org/.
 *    If you don't start sbt with "bin/sbt-batch": Run "npm install jsdom" in this or a parent directory.
 *    This command creates a directory "node_modules" and a file "package-lock.json".
 *
 * Recommended usage for CI server:
 *    sbt clean-publish
 *
 * To build only, without publishing:
 *    sbt clean-build
 *
 * To build only without running tests or publishing:
 *    sbt clean-pack
 *
 * To build and publish to a repository, use:
 *    sbt -DpublishRepository.credentialsFile=... -DpublishRepository.name=... -DpublishRepository.uri=... clean-publish
 *    (publishRepository.name defaults to publishRepository.uri)
 *
 *    Under Windows, if system properties are not accepted, set an environment variable:
 *    set SBT_OPTS=-DpublishRepository.credentialsFile=... -DpublishRepository.name=... -DpublishRepository.uri=...
 *    sbt clean-publish
 *
 * Parallelize testing with: (time-critical test may fail, don't use the machine while building)
 *    sbt -Dtest.parallel ...
 *
 * OUTPUT
 *    The complete installation package (only the files in lib/ are required):
 *    js7-install/target/universal/js7-install-(version).tgz
 *
 *    The JS7 Engine code only, without externals jars:
 *    target/js7-engine-(version).jar
 *
 *    There is also a Docker example:
 *    js7-docker/target/universal/js7-docker-(version).tgz
 */

import BuildUtils.*
import java.nio.file.Files.{createDirectory, isDirectory}
import java.nio.file.Paths
import sbt.Keys.testOptions
import sbt.{Def, file}
import sbtrelease.ReleasePlugin.autoImport.releaseNextVersion
import sbtrelease.{Version, versionFormatError}
import scala.util.control.NonFatal
// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.crossProject

ThisBuild / scalaVersion := "3.5.2"
ThisBuild / usePipelining := version.value.endsWith("-SNAPSHOT")

val rootDirectory = Paths.get(".").toAbsolutePath
lazy val js7Target = {
  val target = rootDirectory.toFile / "target"
  if (!target.exists) createDirectory(target.toPath)
  target
}

val publishRepositoryCredentialsFile = sys.props.get("publishRepository.credentialsFile").map(o => new File(o))
val publishRepositoryName            = sys.props.get("publishRepository.name")
val publishRepositoryUri             = sys.props.get("publishRepository.uri")
// BuildUtils reads more properties.
val isForDevelopment                 = sys.props contains "dev"

addCommandAlias("clean-all"      , "js7-tester/clean; clean")
//addCommandAlias("clean-all"      , "; js7JS/clean; js7-tester/clean; clean")
addCommandAlias("clean-publish"  , "; clean-all; build; publish-all")
addCommandAlias("clean-build"    , "; clean-all; build")
addCommandAlias("clean-build-only", "; clean-all; build-only")
addCommandAlias("clean-pack"     , "; clean-all; compile-only; pack")
addCommandAlias("build"          ,
  if (testParallelization > 1)
    "; test-all; pack"
  else
    "; compile-all; test-all; pack")
addCommandAlias("build-only"     , "; compile-only; pack")
addCommandAlias("compile-all"    , "; Test/compile")
addCommandAlias("compile-only"   , "; compile")
addCommandAlias("test-all"       , "test")
addCommandAlias("pack"           , "Universal/packageZipTarball")
addCommandAlias("publish-all"    , "universal:publish")  // Publishes artifacts too
addCommandAlias("publish-install", "; install/universal:publish; install-docker:universal:publish")
addCommandAlias("TestControllerAgent", "js7-tests/runMain js7.tests.TestControllerAgent --agents=2 --nodes-per-agent=3 --tasks=3 --job-duration=1.5s --period=10.s")
addCommandAlias("quickPublishLocal", "; compile; publishLocal")
//addCommandAlias("quickPublishLocal", "; compile; publishLocal; project js7JS; compile; publishLocal")

ThisBuild / javacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-deprecation",
  "-Xlint:unchecked",
  "-Xdiags:verbose")

//Scala 3? ThisBuild / scalacOptions ++= (if (isForDevelopment) Nil else
//Scala 3?   Seq("-Wconf:cat=unused-imports:error"))

ThisBuild / scalacOptions ++= Seq(
  //"-explain",
  "-java-output-version:17",
  "-deprecation",
  "-feature",
  //"-language:noAutoTupling",
  //"-language:strictEquality",
  //"-Werror",
  //"-Wnonunit-statement",
  "-Wshadow:type-parameter-shadow",
  "-Wunused:imports",
  "-Wunused:implicits",
  //"-Wsafe-init",
  "-Yexplicit-nulls",
  "-Yretain-trees") // Required for Circe derived default values

Global / concurrentRestrictions := Seq(
  Tags.limit(Tags.Test, max = testParallelization),
  Tags.limit(Tags.Compile, max = sys.runtime.availableProcessors),
  Tags.limitAll(if (parallelExecution.value) sys.runtime.availableProcessors max testParallelization else 1))

// https://www.scalatest.org/user_guide/using_scalatest_with_sbt
val scalaTestArguments = Tests.Argument(
  TestFrameworks.ScalaTest,
  ((if (testParallelization > 1) "-oNCLPQF" else "-oF") +: Seq("-W", "30", "30")) *)

val _dummy_ : Unit = {
  sys.props("TEST") = "true"
}

val publishSettings = Seq(
  Compile / packageDoc / publishArtifact := false,
  credentials ++= publishRepositoryCredentialsFile.map(o => Credentials(o)),
  publishTo := publishRepositoryUri.map(uri => publishRepositoryName getOrElse uri at uri))

val commonSettings = Seq(
  organization := "com.sos-berlin.js7.engine",
  organizationName := "Software- und Organisations-Service GmbH, Berlin",
  organizationHomepage := Some(url("https://js7.sh")),
  licenses += "GPLv3" -> url("https://www.gnu.org/licenses/gpl-3.0.txt"),
  pomExtra :=
    <developers>
      <developer>
          <name>Joacim Zschimmer</name>
          <organization>{organizationName.value}</organization>
          <organizationUrl>{organizationHomepage.value.get}</organizationUrl>
      </developer>
    </developers>,
  dependencyOverrides ++= {
    if (sys.props.contains("evictionWarnings"))
      Nil
    else {
      import Dependencies.*
      //? catsEffect ++
        circe ++
        slf4j
      }
  },
  Compile / doc / sources := Nil, // No ScalaDoc
  Test / testOptions := Seq(scalaTestArguments),
  Test / logBuffered := false,  // Recommended for ScalaTest
  publishM2 / test := {},
  // Publish
  Compile / packageDoc / publishArtifact := false,
  credentials += publishRepositoryCredentialsFile.map(o => Credentials(o)),
  publishTo := publishRepositoryUri.map(uri => publishRepositoryName getOrElse uri at uri))

useJGit
//git.uncommittedSignifier := Some("UNCOMMITTED")

lazy val tarOptions: List[String] = try
  if (runProcess("tar", "--help").exists(_.contains("--sort=")))
    List("--sort=name")
  else
    Nil
catch { case NonFatal(t) =>
  println(s"tar --help => $t")
  Nil
}

val universalPluginSettings = Seq(
  Universal / packageZipTarball / universalArchiveOptions :=
    tarOptions ++ (Universal / packageZipTarball / universalArchiveOptions).value)

resolvers += Resolver.mavenLocal

// List each subproject here !!!
// Only these subprojects may be tested and published
lazy val js7Subprojects: Seq[ProjectReference] = {
  import BuildUtils.Implicit.crossProjectToJvmProjectReference
  Seq(
    `js7-agent`,
    `js7-agent-client`,
    `js7-agent-data`,
    `js7-base`,
    `js7-cluster`,
    `js7-cluster-watch`,
    `js7-cluster-watch-api`,
    `js7-common`,
    `js7-common-http`,
    `js7-controller`,
    `js7-controller-client`,
    `js7-core`,
    `js7-data`.jvm,
    `js7-data-for-java`,
    `js7-docker`,
    `js7-install`,
    `js7-journal`,
    `js7-launcher`,
    `js7-launcher-for-java`,
    `js7-launcher-for-windows`,
    `js7-license`,
    `js7-license-fake`,
    `js7-provider`,
    `js7-proxy`,
    `js7-service-pgp`,
    `js7-subagent`,
    `js7-tester`,
    `js7-tests`)
}

lazy val js7 = project.in(file("."))
  .aggregate(js7Subprojects *)
  .settings(
    publish / skip := true)

lazy val all = (project in file("target/project-all"))  // Not the default project
  .aggregate(js7)

lazy val `js7-install` = project
  .dependsOn(
    `js7-engine`,
    `js7-provider`,
    `js7-license-fake`,
    `js7-tests`)
  .settings(commonSettings)
  .enablePlugins(JavaAppPackaging, UniversalDeployPlugin)
  .settings {
    import Dependencies.*
    libraryDependencies ++= log4j ++ lmaxDisruptor
  }
  .settings(
    //skip in publish := true,  // We publish only .tgz and .zip generated by sbt-native-packager / UniversalDeployPlugin
    universalPluginSettings,
    Universal / target := js7Target,
    Universal / topLevelDirectory := Some(s"js7-${version.value}"),
    Universal / mappings := {
      val js7EngineJar = (`js7-engine` / Compile / assembly).value
      import NativePackagerHelper.contentOf
      (Universal / mappings).value
        .filter { case (file, path) =>
          def isNotJs7Subproject =
            !path.contains("com.sos-berlin.js7.engine.js7-") ||
            !path.endsWith(".jar") ||
            // But include these into the tar file:
            path.contains("js7-license-fake") ||
            path.contains("js7-provider")
          def isRelevant = // Ignore irrelevant and testing jars
            path.startsWith("lib/") && !isExcludedJar(path.stripPrefix("lib/"))

          val r = isNotJs7Subproject && isRelevant
          //if (!r) println(s"### - $path")
          r
        }
        // Add our js7-engine.jar with all our relevant subprojects (build above)
        .:+(js7EngineJar -> ("lib/com.sos-berlin.js7.engine." + js7EngineJar.getName))
        // Add other files to get a simple installation
        .++(contentOf((`js7-controller`    / Compile / classDirectory).value / "js7/controller/installation"))
        .++(contentOf((`js7-proxy`.jvm     / Compile / classDirectory).value / "js7/proxy/installation"))
        .++(contentOf((`js7-cluster-watch` / Compile / classDirectory).value / "js7/cluster/watch/installation"))
        .++(contentOf((`js7-provider`      / Compile / classDirectory).value / "js7/provider/installation"))
        .++(contentOf((`js7-agent`         / Compile / classDirectory).value / "js7/agent/installation"))
        .++(contentOf((`js7-subagent`      / Compile / classDirectory).value / "js7/subagent/installation"))
        .++(contentOf((`js7-core`          / Compile / classDirectory).value / "js7/core/installation"))
        .++(contentOf((`js7-base`.jvm      / Compile / classDirectory).value / "js7/base/installation"))
        .filter { case (file, path) =>
          !isDirectory(file.toPath)
        }
        .sortBy(_._2) /*sbt-natice-packager ignores this*/
        //.filter { case (x, path) =>
        //  println(s"### + $path")
        //  true
        //}
    })

/** Dummy subproject to provide the js7-engine.jar containing all relevant subprojects. */
lazy val `js7-engine` = project.in(file("target/js7-engine"))
  .dependsOn(
    `js7-proxy`.jvm,
    `js7-controller`,
    `js7-agent`,
    `js7-subagent`,
    `js7-launcher-for-java`,
    `js7-launcher-for-windows`,
    `js7-service-pgp`,
    `js7-tests`)
  .settings(commonSettings)
  .settings(
    // Provide a single jar with all our relevant subprojects:
    assembly / assemblyJarName := s"js7-engine-${version.value}.jar",
    assembly / assemblyOutputPath := js7Target / (assembly / assemblyJarName).value,
    assemblyPackageScala / assembleArtifact := false /*no scala-library*/ ,
    assemblyPackageDependency / assembleArtifact := false /*no 3rd party libraries*/)

lazy val `js7-docker` = project
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++= log4j ++ lmaxDisruptor
  }
  .enablePlugins(JavaAppPackaging, UniversalDeployPlugin)
  .settings(
    universalPluginSettings,
    Universal / target := js7Target,
    Universal / topLevelDirectory := None,
    Universal / mappings :=
      NativePackagerHelper.contentOf(baseDirectory.value / "src/main/resources/js7/install/docker/")
        .map { case (file, dest) => file -> ("build/" + dest) })

lazy val `js7-tester` = crossProject(JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      "org.typelevel" %%% "cats-core" % catsVersion ++
      "io.circe" %%% "circe-core" % circeVersion ++
      "io.circe" %%% "circe-parser" % circeVersion ++
      "io.circe" %%% "circe-generic" % circeVersion ++
      "org.scalatest" %%% "scalatest" % scalaTestVersion ++
      "org.scalactic" %%% "scalactic" % scalaTestVersion
  }

lazy val `js7-base` = crossProject(JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .dependsOn(`js7-tester` % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      "dev.zio" %%% "izumi-reflect" % izumiReflectVersion ++
      "org.typelevel" %%% "cats-core" % catsVersion ++
      "org.typelevel" %%% "cats-laws" % catsVersion % "test" ++
      "org.typelevel" %%% "cats-parse" % catsParseVersion ++
      "org.typelevel" %%% "cats-effect" % catsEffectVersion ++
      "org.typelevel" %%% "discipline-core" % disciplineVersion % "test" ++
      "org.typelevel" %%% "discipline-scalatest" % disciplineScalaTestVersion % "test" ++
      "io.circe" %%% "circe-core" % circeVersion ++
      "io.circe" %%% "circe-parser" % circeVersion ++
      "io.circe" %%% "circe-generic" % circeVersion ++
      "co.fs2" %% "fs2-core" % fs2Version ++
      "co.fs2" %% "fs2-reactive-streams" % fs2Version ++
      "co.fs2" %% "fs2-io" % fs2Version ++
      "com.lihaoyi" %%% "sourcecode" % sourcecodeVersion ++
      "com.softwaremill.common" %% "tagging" % softwaremillTaggingVersion ++
      findbugs ++
      intelliJAnnotations % "compile" ++
      catsEffectTesting ++
      "org.scalactic" %%% "scalactic" % scalaTestVersion % testWhenIntelliJ ++
      "org.scalatest" %%% "scalatest" % scalaTestVersion % testWhenIntelliJ ++
      "org.scalatestplus" %%% "scalacheck-1-16" % scalaTestCheckVersion % "test" ++
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % "test"
  }
  .jvmSettings {
    import Dependencies.*
    libraryDependencies ++=
      scalaLogging ++ log4j % "test" ++ lmaxDisruptor % "test" ++
      typesafeConfig ++
      log4j ++ lmaxDisruptor
  }
  .jvmSettings(
    Compile / resourceGenerators += Def.task {
      val versionFile = (Compile / resourceManaged).value / "js7/base/installation/VERSION"
      IO.write(versionFile, BuildInfos.info.value.prettyVersion + "\n")

      val log4jFile = (Compile / resourceManaged).value / "log4j2.xml"
      IO.copyFile(
        (Compile / resourceDirectory).value / "js7" / "log4j2.xml",
        log4jFile)

      Seq(versionFile, log4jFile)
    }.taskValue)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoPackage := "js7.base.unused",
    buildInfoUsePackageAsPath := true,
    buildInfoOptions += BuildInfoOption.PackagePrivate,
    buildInfoKeys := BuildInfos.info.value.buildInfoMap.map(BuildInfoKey(_)).toSeq,
    Compile / resourceGenerators += Def.task {
      val file = (Compile / resourceManaged).value / "js7/js7-engine.properties"
      IO.write(file, BuildInfos.info.value.buildPropertiesString)
      Seq(file)
    }.taskValue,
  )

/** js7-build-info provides version info in a Scala-free jar. */
lazy val `js7-build-info` = (project in file("target/project-js7-build-info"))
  .settings(commonSettings)
  .settings(
    crossPaths := false,
    autoScalaLibrary := false)
  .settings(
    Compile / resourceGenerators += Def.task {
      val file = (Compile / resourceManaged).value / "js7/build-info/build-info.properties"
      IO.write(file, BuildInfos.info.value.buildPropertiesString)
      Seq(file)
    }.taskValue)

lazy val `js7-data` = crossProject(JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .dependsOn(`js7-base`, `js7-base` % "test->test", `js7-tester` % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test" ++
    //"org.scalatest" %%% "scalatest-freespec" % scalaTestVersion % "test" ++
      "org.typelevel" %%% "cats-laws" % catsVersion % "test" ++
      "org.typelevel" %%% "discipline-core" % disciplineVersion % "test" ++
      "org.typelevel" %%% "discipline-scalatest" % disciplineScalaTestVersion % "test"
  }

lazy val `js7-data-for-java` = project
  .dependsOn(
    `js7-data`.jvm,
    `js7-data`.jvm % "test->test",
    `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      typesafeConfig ++
      "io.vavr" % "vavr" % vavrVersion ++
      "io.projectreactor" % "reactor-core" % reactorVersion ++
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test" ++
      hamcrest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-common` = project
  .dependsOn(
    `js7-data`.jvm, `js7-data`.jvm % "test->test",
    `js7-base`.jvm, `js7-base`.jvm % "test->test",
    `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      typesafeConfig ++
      pekkoHttp ++
      pekkoActor ++
      pekkoSlf4j ++
      pekkoHttpTestkit % "test" ++
      javaxInject ++
      findbugs ++
      scalaTest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
    }

lazy val `js7-common-http` = crossProject(JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .dependsOn(
    `js7-data`, `js7-data` % "test->test",
    `js7-base`, `js7-base` % "test",
    `js7-tester` % "test")
  .jvmConfigure(_.dependsOn(`js7-common`))
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++= scalaTest % "test"
    libraryDependencies += "org.typelevel" %%% "cats-core" % catsVersion
    libraryDependencies += "org.typelevel" %%% "cats-effect" % catsEffectVersion
  }
  .jvmSettings {
    import Dependencies.*
    libraryDependencies ++=
      pekkoHttp ++
      scalaLogging ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-controller` = project
  .dependsOn(
    `js7-controller-client`.jvm,
    `js7-core`,
    `js7-cluster`,
    `js7-common`,
    `js7-agent-client`,
    `js7-cluster-watch`,
    `js7-data`.jvm % "test->test",
    `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings(
    Compile / packageDoc / mappings := Seq.empty)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      scalaTest % "test" ++
      pekkoHttpTestkit % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-provider` = project
  .dependsOn(`js7-proxy`.jvm, `js7-controller`, `js7-controller-client`.jvm, `js7-core`, `js7-common`,
    `js7-base`.jvm % "test->test",
    `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings(
    Compile / packageDoc / mappings := Seq.empty)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      scalaTest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-proxy` = crossProject(JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .dependsOn(
    `js7-controller-client`,
    `js7-agent-data`,
    `js7-base` % "test->test",
    `js7-tester` % "test")
  .jvmConfigure(_.dependsOn(
    `js7-data-for-java`,
    `js7-cluster-watch`))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= {
      import Dependencies.*
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test" ++
      Nil/*++
      "org.scalatest" %%% "scalatest-freespec" % scalaTestVersion % "test"*/
    })
  .jvmSettings(
    libraryDependencies ++= {
      import Dependencies.*
      pekkoHttp ++
      hamcrest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
    })

lazy val `js7-controller-client` = crossProject(JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .dependsOn(
    `js7-cluster-watch-api`,
    `js7-agent-data`,
    `js7-common-http`,
    `js7-base` % "test->test",
    `js7-tester` % "test")
  .jvmConfigure(_.dependsOn(`js7-common`))
  .settings(commonSettings)
  .settings(
    libraryDependencies += {
      import Dependencies.*
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test" /*++
      "org.scalatest" %%% "scalatest-freespec" % scalaTestVersion % "test"*/
    })
  .jvmSettings(
    libraryDependencies ++= {
      import Dependencies.*
      pekkoHttp ++
      log4j % "test" ++
      lmaxDisruptor % "test"
    })

lazy val `js7-core` = project
  .dependsOn(`js7-journal`, `js7-common`, `js7-license`,
    `js7-base`.jvm % "test->test", `js7-tester`.jvm % "test",
    `js7-service-pgp` % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      pekkoHttpTestkit % "test" ++
      scalaTest % "test" ++
      scalaCheck % "test" ++ log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-launcher` = project
  .dependsOn(`js7-launcher-for-windows`, `js7-core`, `js7-base`.jvm % "test->test", `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      scalaTest % "test" ++
      scalaCheck % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-launcher-for-java` = project
  .dependsOn(`js7-launcher`, `js7-data-for-java`, `js7-base`.jvm % "test->test", `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      "io.vavr" % "vavr" % vavrVersion ++
      hamcrest % "test" ++
      scalaTest % "test" ++
      scalaCheck % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-launcher-for-windows` = project
  .dependsOn(`js7-base`.jvm, `js7-base`.jvm % "test->test", `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      jna ++
      scalaTest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-journal` = project
  .dependsOn(`js7-common-http`.jvm, `js7-common`, `js7-base`.jvm % "test->test", `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      pekkoHttp ++
      pekkoHttpTestkit % "test" ++
      tagging ++
      scalaTest % "test" ++
      scalaCheck % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-cluster` = project
  .dependsOn(
    `js7-cluster-watch-api`.jvm,
    `js7-core`,
    `js7-common-http`.jvm,
    `js7-common`,
    `js7-base`.jvm % "test->test",
    `js7-tester`.jvm % "test",
    `js7-license-fake` % "test->test",
    `js7-journal` % "test->test")
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      pekkoHttpTestkit % "test" ++
      scalaTest % "test" ++
      scalaCheck % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-cluster-watch-api` = crossProject(JVMPlatform)
  .dependsOn(
    `js7-data`/*TODO move js7.data.cluster.* here*/,
    `js7-common-http`,
    `js7-base`,
    `js7-base` % "test->test",
    `js7-tester` % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      scalaTest % "test" ++
      scalaCheck % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-cluster-watch` = project
  .dependsOn(
    `js7-cluster-watch-api`.jvm,
    `js7-common`,
    `js7-base`.jvm % "test->test",
    `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      scalaTest % "test" ++
      scalaCheck % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-agent` = project
  .dependsOn(
    `js7-agent-client`,
    `js7-subagent`,
    `js7-cluster`,
    `js7-agent-data`.jvm,
    `js7-launcher`,
    `js7-core`,
    `js7-common`,
    `js7-data`.jvm,
    `js7-base`.jvm % "test->test", `js7-tester`.jvm % "test",
    `js7-service-pgp` % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      findbugs ++
      pekkoActor ++
      pekkoStream ++
      pekkoSlf4j ++
      pekkoHttpTestkit % "test" ++
      pekkoHttp ++
      intelliJAnnotations % "compile" ++
      scalaTest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-subagent` = project
  .dependsOn(
    `js7-launcher`,
    `js7-common`,
    `js7-data`.jvm,
    `js7-base`.jvm % "test->test",
    `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      intelliJAnnotations % "compile" ++
      scalaTest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-agent-client` = project
  .dependsOn(`js7-data`.jvm, `js7-common-http`.jvm, `js7-common`, `js7-agent-data`.jvm,
    `js7-cluster-watch-api`.jvm,
    `js7-base`.jvm % "test->test",
    `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings(description := "JS7 Agent - Client")
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      pekkoActor ++
      pekkoHttp ++
      scalaTest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-agent-data` = crossProject(JVMPlatform)
  .dependsOn(
    `js7-data`,
    `js7-base` % "test->test",
    `js7-tester` % "test")
  .settings(commonSettings)
  .settings(description := "JS7 Agent - Value Classes")
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      findbugs ++
      intelliJAnnotations % "compile" ++
      scalaTest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-license` = project
  .settings(commonSettings)

lazy val `js7-license-fake` = project
  .dependsOn(`js7-license`)
  .settings(commonSettings)

lazy val `js7-service-pgp` = project
  .dependsOn(
    `js7-base`.jvm,
    `js7-base`.jvm % "test->test",
    `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies.*
    libraryDependencies += bouncyCastle
  }

lazy val `js7-tests` = project
  .dependsOn(
    `js7-controller`,
    `js7-agent`,
    `js7-agent` % "test->test",
    `js7-proxy`.jvm,
    `js7-cluster-watch`,
    `js7-agent-client`,
    `js7-core` % "test->test",
    `js7-data`.jvm % "test->test",
    `js7-base`.jvm,
    `js7-provider`,
    `js7-base`.jvm % "test->test",
    `js7-tester`.jvm % "test",
    `js7-docker` % "test",
    `js7-launcher-for-java` % "test->test",
    `js7-launcher-for-windows` % "test->test",
    `js7-license-fake`,
    `js7-service-pgp`)
  .settings(
    commonSettings,
    publish / skip := true,
    description := "JS7 Tests")
  .settings {
    import Dependencies.*
    libraryDependencies ++=
      pekkoHttpTestkit % "test" ++  // For IntelliJ IDEA 2018.2
      scalaTest ++
      hamcrest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

def isExcludedJar(path: String) =
  path.startsWith("com.google.code.findbugs.jsr305-") ||
  path.startsWith("org.scalatest.")
  //path.startsWith("com.intellij.annotations-") ||  <-- required by TypeTag[RichProcess]

//--------------------------------------------------------------------------------------------------
// RELEASE

releaseTagComment        := s"Version ${version.value}"
releaseCommitMessage     := s"Version ${version.value}"
releaseNextCommitMessage := s"Version ${version.value}"
val release = sys.props.contains("js7.release")

releaseVersion := (v =>
  Version(v).fold(versionFormatError(v)) { currentVersion =>
    if (release) {
      if (!currentVersion.unapply.endsWith("-SNAPSHOT")) {
        sys.error(s"Current version must end with -SNAPSHOT: $currentVersion")
      }
      currentVersion.withoutQualifier.unapply
    } else {
      val prerelease = {
        val commitDate = BuildInfos.committedAt.value
          .map(_.toString)
          .getOrElse(sys.error("gitHeadCommitDate returned None (no Git?)"))
          .take(10)/*yyyy-mm-dd*/
        // Remove hyphens according to Semantic Versioning pre-release syntax
        val yyyymmdd = commitDate.substring(0, 4) + commitDate.substring(5, 7) + commitDate.substring(8, 10)
        "beta." + yyyymmdd
      }
      val version = currentVersion.withoutQualifier.unapply + "-" + prerelease
      var v = version
      var i = 0
      val tags = runProcess("git", "tag").toSet
      while (tags contains s"v$v") {
        i += 1
        v = s"$version.$i"
      }
      v
    }
  })

val VersionPattern = """([0-9]+)\.([0-9]+)\.([0-9]+)(?:-.*)?""".r

releaseNextVersion := {
  case v if !release =>
    Version(v).fold(versionFormatError(v))(_.withoutQualifier.unapply + "-SNAPSHOT")

  case VersionPattern(major, minor, patch) =>
    s"$major.$minor.${patch.toInt + 1}-SNAPSHOT"

  case _ => sys.error(s"Current version does not match $VersionPattern: $version")
}

releaseProcess := {
  import sbtrelease.ReleaseStateTransformations.{checkSnapshotDependencies, commitNextVersion, commitReleaseVersion, inquireVersions, runTest, setNextVersion, setReleaseVersion, tagRelease}
  // See https://github.com/sbt/sbt-release#can-we-finally-customize-that-release-process-please
  Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    //runClean,  // This deletes BuildInfo and disturbs IntelliJ. Users should clean themselves!
    runTest,
    setReleaseVersion,
    commitReleaseVersion,       // performs the initial git checks
    tagRelease,
  //publishArtifacts,           // checks whether `publishTo` is properly set up
    setNextVersion,
    commitNextVersion)
    //pushChanges)                // also checks that an upstream branch is properly configured
}
