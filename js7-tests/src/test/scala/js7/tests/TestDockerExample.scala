package js7.tests

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO, Resource, ResourceIO}
import cats.syntax.all.*
import java.nio.file.Files.{createDirectories, createDirectory, setPosixFilePermissions}
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import js7.agent.TestAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.catsutils.OurApp
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.generic.SecretString
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{deleteDirectoryContentRecursively, deleteDirectoryRecursively, temporaryDirectory}
import js7.base.log.Log4j
import js7.common.commandline.CommandLineArguments
import js7.common.system.startup.JavaMain
import js7.common.system.startup.JavaMain.runMain
import js7.controller.RunningController
import js7.controller.configuration.ControllerConfiguration
import js7.controller.tests.TestDockerEnvironment
import js7.data.agent.AgentPath
import js7.provider.Provider
import js7.provider.configuration.ProviderConfiguration
import js7.service.pgp.PgpCommons.{RichPGPPublicKey, toArmoredAsciiBytes}
import js7.service.pgp.PgpSigner
import scala.util.chaining.scalaUtilChainingOps

object TestDockerExample extends OurApp:

  private val TestAgentPaths = List(AgentPath("agent-1"), AgentPath("agent-2"))

  private given IORuntime = runtime

  def run(args: List[String]) =
    runMain:
      IO.defer:
        val directory =
          (temporaryDirectory / "TestDockerExample").tap: directory =>
            println(s"Using directory $directory")
            if !Files.exists(directory) then
              createDirectory(directory)
            else
              println(s"Deleting $directory")
              deleteDirectoryContentRecursively(directory)
        run(directory)
          .guarantee:
            IO(Log4j.shutdown())

  private def run(directory: Path): IO[ExitCode] =
    val env = new TestDockerEnvironment(TestAgentPaths, directory)
    val (signer, verifiy) = PgpSigner.forTest(SecretString("PGP-PASSWORD"))

    def provide(path: String) =
      val dir = if path.startsWith("subagent") then env.agentsDir else directory
      createDirectories((dir / path).getParent)
      JavaResource(s"js7/install/docker/volumes/$path").copyToFile(dir / path)
      if path.contains("/executables/") then
        setPosixFilePermissions(dir / path, PosixFilePermissions.fromString("rwx------"))

    provide("controller/config/private/private.conf")
    directory / "controller/config/private/private.conf" ++=
      s"""js7.auth.users {
         |  Provider {
         |    password = "plain:Provider PASSWORD"
         |    permissions = [ UpdateItem ]
         |  }
         |}
      """.stripMargin

    provide("subagent-1a/config/private/private.conf")
    provide("subagent-1a/config/executables/test")
    provide("subagent-1b/config/private/private.conf")
    provide("subagent-1c/config/private/private.conf")
    provide("subagent-2a/config/private/private.conf")
    env.controllerDir / "config" / "controller.conf" :=
      """js7.web.server.auth.loopback-is-public = on
        |""".stripMargin
    createDirectory(directory / "controller/config/private/trusted-pgp-keys")
    directory / "controller/config/private/trusted-pgp-keys" / "pgp.asc" :=
      signer.pgpSecretKey.getPublicKey.toArmoredAsciiBytes
    directory / "controller/config/private/private.conf" ++=
      s"""js7.configuration.trusted-signature-keys.PGP = $${js7.config-directory}"/private/trusted-pgp-keys"
         |""".stripMargin
    for subagent <- Seq("subagent-1a", "subagent-1b", "subagent-1c", "subagent-2a") do
      createDirectory(env.agentsDir / s"$subagent/config/private/trusted-pgp-keys")
      env.agentsDir / s"$subagent/config/private/trusted-pgp-keys" / "test.asc" :=
        signer.pgpSecretKey.getPublicKey.toArmoredAsciiBytes
      env.agentsDir / s"$subagent/config/private/private.conf" ++=
        s"""js7.configuration.trusted-signature-keys.PGP = $${js7.config-directory}"/private/trusted-pgp-keys"
           |""".stripMargin

    val conf = ControllerConfiguration.forTest(
      configAndData = env.controllerDir,
      httpPort = Some(4444))
    val controllerUri = conf.webServerPorts.head.uri

    val providerEnv: ResourceIO[Path] =
      Resource:
        IO:
          val dir = directory / "provider"
          createDirectory(dir)
          provide("provider/config/order-generators/test.order.xml")
          provide("provider/config/live/მაგალითად.workflow.json")
          provide("provider/config/live/agent-1.agent.json")
          provide("provider/config/live/agent-2.agent.json")
          provide("provider/config/live/subagent-1a.subagent.json")
          provide("provider/config/live/subagent-1b.subagent.json")
          provide("provider/config/live/subagent-1c.subagent.json")
          provide("provider/config/live/subagent-2a.subagent.json")
          createDirectories(dir / "config/private")
          dir / "config/private/private-pgp-key.asc" := signer.pgpSecretKey.toArmoredAsciiBytes
          dir / "config/private/private.conf" :=
            s"""js7.provider.sign-with = PGP
               |js7.provider.private-signature-keys.PGP {
               |  key = $${js7.config-directory}"/private/private-pgp-key.asc"
               |  password = "PGP-PASSWORD"
               |}
               |js7.provider.controller.user = "Provider"
               |js7.provider.controller.password = "Provider PASSWORD"
               """.stripMargin
          dir -> IO(deleteDirectoryRecursively(dir))

    val providerResource: ResourceIO[Provider] =
      for
        given IORuntime <- RunningController.ioRuntimeResource[IO](conf)
        _ <- providerEnv
        provider <- Provider.resource:
          ProviderConfiguration.fromCommandLine(
            CommandLineArguments(Seq(
              s"--config-directory=$directory/provider/config",
              s"--controller-uri=$controllerUri")),
            addConfig = config"""
              js7.provider.add-orders-every = 10s
              js7.provider.add-orders-earlier = 1s
              """)
      yield
        provider

    val controllerResource: ResourceIO[RunningController] =
      for
        given IORuntime <- RunningController.ioRuntimeResource[IO](conf)
        controller <- RunningController.resource(conf)
      yield
        controller

    def subagentResource(agentPath: AgentPath): ResourceIO[TestAgent] =
      TestAgent.resource(AgentConfiguration.forTest(
        configAndData = env.agentDir(agentPath),
        name = agentPath.string))

    val subagentResources: List[ResourceIO[TestAgent]] =
      TestAgentPaths.map(subagentResource)

    (providerResource :: controllerResource :: subagentResources)
      .sequence.use: services =>
        services.parTraverse:
          _.untilTerminated.map(_.toExitCode)
      .map:
        _.find(_ != ExitCode.Success) getOrElse ExitCode.Success
  end run
