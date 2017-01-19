import scala.collection.immutable.Seq

val commonSettings = Seq(
  organization := "com.sos-berlin.jobscheduler.engine",
  organizationName := "SOS Software GmbH, Berlin",
  organizationHomepage := Some(url("https://www.sos-berlin.com")),
  scalaVersion := Libraries.scalaVersion,
  logBuffered in Test := false)

val ParallelTestsLimit = math.max(1, sys.runtime.availableProcessors / 2)
concurrentRestrictions in Global += Tags.limit(Tags.Test, ParallelTestsLimit)

resolvers += Resolver.mavenLocal

lazy val jobscheduler = (project in file("."))
  .aggregate(base, data, common)
  .settings(commonSettings: _*)
  .settings(publishM2 := {})  // This project is only a build wrapper

lazy val base = project
  .settings(commonSettings: _*)
  .settings {
    import Libraries._
    libraryDependencies ++=
      scalaXml ++
      sprayJsons ++
      scalaTest % "test" ++
      googleFindbugs
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
      slf4j ++
      javaxInject ++
      guice ++
      typesafeConfig ++
      akkaActor ++
      sprayCan ++
      sprayHttpx ++
      sprayRouting ++
      sprayJsons ++
      snakeYaml ++
      guava ++
      intelliJAnnotations % "compile" ++
      googleFindbugs ++
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
