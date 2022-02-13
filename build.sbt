// WARNING: Start only in a secure closed environment (like Docker) !!!
// because tests open localhost TCP ports that may allow code injection.
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
  * To release an alpha version
  *   Only if the current branch is not "main" and starts not with "release/" !
  *   sbt release use-defaults
  *
  * sbt allows to preset these command line options in the environment variable SBT_OPTS.
  */
import BuildUtils._
import java.nio.file.Paths
import sbt.Keys.testOptions
import sbt.file
import sbtrelease.ReleasePlugin.autoImport.releaseNextVersion
import sbtrelease.{Version, versionFormatError}
// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.crossProject

val rootDirectory = Paths.get(".").toAbsolutePath

val publishRepositoryCredentialsFile = sys.props.get("publishRepository.credentialsFile").map(o => new File(o))
val publishRepositoryName            = sys.props.get("publishRepository.name")
val publishRepositoryUri             = sys.props.get("publishRepository.uri")
// BuildUtils reads more properties.
val isForDevelopment                 = sys.props contains "dev"

addCommandAlias("clean-all"      , "; js7JS/clean; js7-tester/clean; clean")
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
addCommandAlias("pack"           , "universal:packageZipTarball")
addCommandAlias("publish-all"    , "universal:publish")  // Publishes artifacts too
addCommandAlias("publish-install", "; install/universal:publish; install-docker:universal:publish")
addCommandAlias("TestControllerAgent", "js7-tests/runMain js7.tests.TestControllerAgent --agents=2 --nodes-per-agent=3 --tasks=3 --job-duration=1.5s --period=10.s")
addCommandAlias("quickPublishLocal", "; compile; publishLocal; project js7JS; compile; publishLocal")

//scalafixDependencies in ThisBuild += "org.scala-lang.modules" %% "scala-collection-migrations" % "2.1.4"
//addCompilerPlugin(scalafixSemanticdb)
//ThisBuild / scalacOptions ++= Seq("-P:semanticdb:synthetics:on", "-Yrangepos"/*required by SemanticDB compiler plugin*/)

//scalafixDependencies in ThisBuild += "org.scalatest" %% "autofix" % "3.1.0.0"
//addCompilerPlugin(scalafixSemanticdb) // enable SemanticDB
val jdkVersion = "1.8"

ThisBuild / scalacOptions ++= Seq(
  "-explaintypes",
  "-Ymacro-annotations",
  "-explaintypes",
  "-feature",
  "-deprecation",
  //"-Wunused:privates",
  //"-Wunused:locals",
  //"-Wunused:implicits",
  "-Xlint:infer-any",
  "-Xlint:doc-detached",
  "-Xlint:private-shadow",
  //"-Xlint:type-parameter-shadow",
  "-Xlint:implicit-not-found",
  "-Xlint:eta-zero",

  "-unchecked",
  "-Xcheckinit",
  "-Xlint:adapted-args",
  "-Xlint:constant",
  "-Xlint:delayedinit-select",
  "-Xlint:deprecation",
  "-Xlint:doc-detached",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  //"-Xlint:missing-interpolator",
  //"-Xlint:nullary-override",
  "-Xlint:nullary-unit",
  "-Xlint:option-implicit",
  //"-Xlint:package-object-classes",
  "-Xlint:poly-implicit-overload",
  //"-Xlint:private-shadow",
  "-Xlint:stars-align",
  "-Xlint:type-parameter-shadow",
  "-Wdead-code",
  "-Wextra-implicit",
  //"-Wnumeric-widen",
  //"-Wunused:implicits",
  "-Wunused:imports",
  //"-Wunused:locals",
  //"-Wunused:params",
  //"-Wunused:patvars",
  //"-Wunused:privates",
  //"-Wvalue-discard",
  )

Global / concurrentRestrictions := Seq(
  Tags.limit(Tags.Test, max = testParallelization),
  Tags.limit(Tags.Compile, max = sys.runtime.availableProcessors),
  Tags.limitAll(if (parallelExecution.value) sys.runtime.availableProcessors max testParallelization else 1))

// http://www.scalatest.org/user_guide/using_scalatest_with_sbt
val scalaTestArguments = Tests.Argument(TestFrameworks.ScalaTest,
  (if (testParallelization > 1) "-oNCLPQF" else "-oF") +: Seq("-W", "30", "30"): _*)

val _dummy_ = {
  sys.props("TEST") = "true"
}

val publishSettings = Seq(
  Compile / packageDoc / publishArtifact := false,
  credentials ++= publishRepositoryCredentialsFile.map(o => Credentials(o)),
  publishTo := publishRepositoryUri.map(uri => publishRepositoryName getOrElse uri at uri))

val commonSettings = Seq(
  organization := "com.sos-berlin.js7.engine",
  organizationName := "Software- und Organsiations-Service GmbH, Berlin",
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
  scalaVersion := Dependencies.scalaVersion,
  Compile / javacOptions ++= Seq("-encoding", "UTF-8", "-source", jdkVersion),  // This is for javadoc, too
  Compile / compile / javacOptions ++= Seq(
    "-target", jdkVersion, "-deprecation", "-Xlint:all", "-Xlint:-serial", "-Xdiags:verbose"),
  dependencyOverrides ++= {
    if (sys.props.contains("evictionWarnings"))
      Nil
    else {
      import Dependencies._
      cats ++
        ("org.typelevel" %% "cats-core" % catsVersion) ++
        ("org.typelevel" %% "cats-effect" % catsEffectVersion) ++
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
git.uncommittedSignifier := Some("UNCOMMITTED")

val universalPluginSettings = Seq(
  Universal / packageZipTarball / universalArchiveOptions :=
    (Universal / packageZipTarball / universalArchiveOptions).value)

resolvers += Resolver.mavenLocal

lazy val js7 = (project in file("."))
  .aggregate(
    `js7-agent`,
    `js7-subagent`,
    `js7-base`.jvm,
    `js7-common`,
    `js7-common-http`.jvm,
    `js7-core`,
    `js7-launcher`,
    `js7-launcher-for-java`,
    `js7-launcher-for-windows`,
    `js7-data`.jvm,
    `js7-data-for-java`,
    `js7-docker`,
    `js7-install`,
    `js7-journal`,
    `js7-cluster`,
    `js7-controller`,
    `js7-controller-client`.jvm,
    `js7-agent-client`,
    `js7-agent-data`,
    `js7-provider`,
    `js7-proxy`.jvm,
    `js7-tests`,
    `js7-license`,
    `js7-license-fake`,
    `js7-build-info`)
  .settings(publish / skip := true)

lazy val js7JS = (project in file("target/project-js7JS"))
  .aggregate(
    `js7-base`.js,
    `js7-common-http`.js,
    `js7-data`.js,
    `js7-controller-client`.js,
    `js7-proxy`.js)
  .settings(publish / skip := true)

lazy val all = (project in file("target/project-all"))  // Not the default project
  .aggregate(js7, js7JS)

lazy val `js7-install` = project
  .dependsOn(`js7-controller`, `js7-provider`, `js7-agent`, `js7-launcher-for-java`, `js7-tests`, `js7-license-fake`)
  .settings(commonSettings)
  .enablePlugins(JavaAppPackaging, UniversalDeployPlugin)
  .settings {
    import Dependencies._
    libraryDependencies ++= log4j ++ lmaxDisruptor
  }
  .settings(
    //skip in publish := true,  // We publish only .tgz and .zip generated by sbt-native-packager / UniversalDeployPlugin
    universalPluginSettings,
    Universal / topLevelDirectory := Some(s"js7-${version.value}"),
    Universal / mappings :=
      (((Universal / mappings).value filter { case (_, path) => (path startsWith "lib/") && !isExcludedJar(path stripPrefix "lib/") }) ++
        NativePackagerHelper.contentOf((`js7-controller` / Compile / classDirectory).value / "js7/controller/installation") ++
        NativePackagerHelper.contentOf((`js7-provider` / Compile / classDirectory).value / "js7/provider/installation") ++
        NativePackagerHelper.contentOf((`js7-agent` / Compile / classDirectory).value / "js7/agent/installation") ++
        NativePackagerHelper.contentOf((`js7-core` / Compile / classDirectory).value / "js7/core/installation") ++
        NativePackagerHelper.contentOf((`js7-base`.jvm / Compile / classDirectory).value / "js7/base/installation")
      ).toVector.sortBy(_._2))

lazy val `js7-docker` = project
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++= log4j ++ lmaxDisruptor
  }
  .enablePlugins(JavaAppPackaging, UniversalDeployPlugin)
  .settings(
    universalPluginSettings,
    Universal / topLevelDirectory := None,
    Universal / mappings :=
      NativePackagerHelper.contentOf(baseDirectory.value / "src/main/resources/js7/install/docker/")
        .map { case (file, dest) => file -> ("build/" + dest) })

lazy val `js7-tester` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      diffx ++
      "com.outr" %%% "scribe" % scribeVersion ++
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
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaReflect ++  // TODO Avoid JVM dependencies here
      "dev.zio" %%% "izumi-reflect" % izumiReflectVersion ++
      "org.typelevel" %%% "cats-core" % catsVersion ++
      "org.typelevel" %%% "cats-effect" % catsEffectVersion ++
      "org.typelevel" %%% "cats-laws" % catsVersion % "test" ++
      "org.typelevel" %%% "discipline-core" % disciplineVersion % "test" ++
      "org.typelevel" %%% "discipline-scalatest" % disciplineScalaTestVersion % "test" ++
      "io.circe" %%% "circe-core" % circeVersion ++
      "io.circe" %%% "circe-parser" % circeVersion ++
      "io.circe" %%% "circe-generic" % circeVersion ++
      "io.circe" %%% "circe-generic-extras" % circeVersion ++
      "io.monix" %%% "monix-eval" % monixVersion ++
      "io.monix" %%% "monix-reactive" % monixVersion ++
      "com.github.mpilquist" %% "simulacrum" % simulacrumVersion ++
      "com.lihaoyi" %%% "sourcecode" % sourcecodeVersion ++
      "com.outr" %%% "scribe" % scribeVersion ++
      "org.scalactic" %%% "scalactic" % scalaTestVersion % Test ++
      findbugs ++
      intelliJAnnotations % "compile" ++
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test" ++
    //"org.scalatest" %%% "scalatest-freespec" % scalaTestVersion % "test" ++
      "org.scalatestplus" %%% "scalacheck-1-14" % scalaTestCheckVersion % "test" ++
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % "test"
  }
  .jvmSettings {
    import Dependencies._
    libraryDependencies ++=
      scalaLogging ++ log4j % "test" ++ lmaxDisruptor % "test" ++
      typesafeConfig
  }
  .jvmSettings(
    Compile / resourceGenerators += Def.task {
      val versionFile = (Compile / resourceManaged).value / "js7/base/installation/VERSION"
      IO.write(versionFile, BuildInfos.longVersion.value + "\n")
      Seq(versionFile)
    }.taskValue)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoPackage := "js7.base",
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := BuildInfos.buildInfoMap.value.map(BuildInfoKey(_)).toSeq)

/** js7-build-info provides version info in a Scala-free jar. */
lazy val `js7-build-info` = (project in file("target/project-js7-build-info"))
  .settings(commonSettings)
  .settings(
    crossPaths := false,
    autoScalaLibrary := false)
  .settings(
    Compile / resourceGenerators += Def.task {
      val file = (Compile / resourceManaged).value / "js7/build-info/build-info.properties"
      IO.write(
        file,
        BuildInfos.buildInfoMap.value
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
  .dependsOn(`js7-base`, `js7-base` % "test->test", `js7-tester` % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      "com.lihaoyi" %%% "fastparse" % fastparseVersion ++
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
    import Dependencies._
    libraryDependencies ++=
      typesafeConfig ++
      "io.vavr" % "vavr" % vavrVersion ++
      "io.projectreactor" % "reactor-core" % reactorVersion ++
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test" ++
      "org.scala-lang.modules" %% "scala-java8-compat" % scalaJava8Version ++
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
    import Dependencies._
    libraryDependencies ++=
      typesafeConfig ++
      bouncyCastle ++
      akkaHttp ++
      akkaActor ++
      akkaSlf4j ++
      akkaHttpTestkit % "test" ++
      javaxInject ++
      guice ++
      findbugs ++
      scalaTest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
    }
  .enablePlugins(GitVersioning)

lazy val `js7-common-http` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .dependsOn(
    `js7-data`, `js7-data` % "test->test",
    `js7-base`,
    `js7-tester` % "test")
  .jvmConfigure(_.dependsOn(`js7-common`))
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
      scalaLogging ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }
  .jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % Dependencies.scalaJsDomVersion)

lazy val `js7-controller` = project
  .dependsOn(
    `js7-controller-client`.jvm,
    `js7-core`,
    `js7-cluster`,
    `js7-common`,
    `js7-agent-client`,
    `js7-data`.jvm % "test->test",
    `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings(
    Compile / packageDoc / mappings := Seq.empty)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaTest % "test" ++
      akkaHttpTestkit % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-provider` = project
  .dependsOn(`js7-proxy`.jvm, `js7-controller`, `js7-controller-client`.jvm, `js7-core`, `js7-common`, `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings(
    Compile / packageDoc / mappings := Seq.empty)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaTest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-proxy` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .dependsOn(`js7-controller-client`, `js7-tester` % "test")
  .jvmConfigure(_.dependsOn(`js7-data-for-java`))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= {
      import Dependencies._
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test" ++
      Nil/*++
      "org.scalatest" %%% "scalatest-freespec" % scalaTestVersion % "test"*/
    })
  .jvmSettings(
    libraryDependencies ++= {
      import Dependencies._
      akkaHttp ++
      "org.scala-lang.modules" %% "scala-java8-compat" % scalaJava8Version ++
      hamcrest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
    })

lazy val `js7-controller-client` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .dependsOn(`js7-common-http`, `js7-tester` % "test")
  .jvmConfigure(_.dependsOn(`js7-common`))
  .settings(commonSettings)
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
      log4j % "test" ++
      lmaxDisruptor % "test"
    })

lazy val `js7-core` = project
  .dependsOn(`js7-journal`, `js7-common`, `js7-license`, `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      shapeless ++
      diffx ++
      akkaHttpTestkit % "test" ++
      scalaTest % "test" ++
      scalaCheck % "test" ++ log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-launcher` = project
  .dependsOn(`js7-launcher-for-windows`, `js7-core`, `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      scalaTest % "test" ++
      scalaCheck % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-launcher-for-java` = project
  .dependsOn(`js7-launcher`, `js7-data-for-java`, `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      "io.vavr" % "vavr" % vavrVersion ++
      hamcrest % "test" ++
      scalaTest % "test" ++
      scalaCheck % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-launcher-for-windows` = project
  .dependsOn(`js7-base`.jvm, `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      jna ++
      scalaTest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-journal` = project
  .dependsOn(`js7-common-http`.jvm, `js7-common`, `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      akkaHttp ++
      akkaHttpTestkit % "test" ++
      guice ++
      diffx ++
      scalaTest % "test" ++
      scalaCheck % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-cluster` = project
  .dependsOn(`js7-core`, `js7-common-http`.jvm, `js7-common`, `js7-tester`.jvm % "test", `js7-license-fake` % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      diffx ++
      scalaTest % "test" ++
      scalaCheck % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-agent` = project
  .dependsOn(
    `js7-subagent`,
    `js7-agent-data`, `js7-launcher`, `js7-core`, `js7-common`, `js7-data`.jvm,
    `js7-agent-client` % "test", `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      findbugs ++
      akkaActor ++
      akkaStream ++
      akkaSlf4j ++
      akkaHttpTestkit % "test" ++
      akkaHttp ++
      intelliJAnnotations % "compile" ++
      guice ++
      scalaTest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-subagent` = project
  .dependsOn(
    `js7-launcher`,
    `js7-common`,
    `js7-agent-data`,
    `js7-data`.jvm,
    `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings {
    import Dependencies._
    libraryDependencies ++=
      intelliJAnnotations % "compile" ++
      scalaTest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-agent-client` = project
  .dependsOn(`js7-data`.jvm, `js7-common-http`.jvm, `js7-common`, `js7-agent-data`,
    `js7-tester`.jvm % "test")
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
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

lazy val `js7-agent-data` = project
  .dependsOn(`js7-common`, `js7-data`.jvm, `js7-tester`.jvm % "test")
  .settings(commonSettings)
  .settings(description := "JS7 Agent - Value Classes")
  .settings {
    import Dependencies._
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

lazy val `js7-tests` = project
  .dependsOn(`js7-controller`, `js7-agent`, `js7-proxy`.jvm, `js7-agent-client`,
    `js7-core` % "test->test",
    `js7-data`.jvm % "test->test",
    `js7-provider`,
    `js7-tester`.jvm % "test",
    `js7-docker` % "test",
    `js7-launcher-for-java` % "test->test",
    `js7-launcher-for-windows` % "test->test",
    `js7-license-fake`)
  .settings(
    commonSettings,
    publish / skip := true,
    description := "JS7 Tests")
  .settings {
    import Dependencies._
    libraryDependencies ++=
      akkaHttpTestkit % "test" ++  // For IntelliJ IDEA 2018.2
      scalaTest % "test" ++
      diffx % "test" ++
      diffxScalaTest % "test" ++
      hamcrest % "test" ++
      log4j % "test" ++
      lmaxDisruptor % "test"
  }

def isExcludedJar(path: String) =
  path.startsWith("com.sos-berlin.js7.engine.js7-install-") ||
  path.startsWith("org.checkerframework.checker-qual-") ||
  path.startsWith("com.google.code.findbugs.jsr305-") ||
  path.startsWith("com.google.errorprone.error_prone_annotations-") ||
  path.startsWith("com.google.guava.listenablefuture-9999.0-empty-to-avoid-conflict-with-guava.jar") ||
  path.startsWith("com.google.j2objc.j2objc-annotations-") ||
  //path.startsWith("com.intellij.annotations-") ||  <-- required by TypeTag[RichProcess]
  path.startsWith("listenablefuture-9999.0-empty-to-avoid-conflict-with-guava-") ||
  path.startsWith("org.typelevel.simulacrum-scalafix-annotations_")

//--------------------------------------------------------------------------------------------------
// RELEASE

releaseTagComment        := s"Version ${version.value}"
releaseCommitMessage     := s"Version ${version.value}"
releaseNextCommitMessage := s"Version ${version.value}"
val release = sys.props.contains("js7.release")

releaseVersion := (v =>
  Version(v).fold(versionFormatError(v)) { currentVersion =>
    if (release) {
      if (!currentVersion.string.endsWith("-SNAPSHOT")) {
        sys.error(s"Current version must end with -SNAPSHOT: $currentVersion")
      }
      currentVersion.withoutQualifier.string
    } else {
      val prerelease = {
        val commitDate = BuildInfos.committedAt.value
          .getOrElse(sys.error("gitHeadCommitDate returned None (no Git?)"))
          .take(10)/*yyyy-mm-dd*/
        // Remove hyphens according to Semantic Versioning pre-release syntax
        val yyyymmdd = commitDate.substring(0, 4) + commitDate.substring(5, 7) + commitDate.substring(8, 10)
        "beta." + yyyymmdd
      }
      val version = currentVersion.withoutQualifier.string + "-" + prerelease
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
    Version(v).fold(versionFormatError(v))(_.withoutQualifier.string + "-SNAPSHOT")

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
    //runClean,  // This deletes BuildInfo and disturbs IntelliJ. Users should clean themself!
    runTest,
    setReleaseVersion,
    commitReleaseVersion,       // performs the initial git checks
    tagRelease,
  //publishArtifacts,           // checks whether `publishTo` is properly set up
    setNextVersion,
    commitNextVersion)
    //pushChanges)                // also checks that an upstream branch is properly configured
}
