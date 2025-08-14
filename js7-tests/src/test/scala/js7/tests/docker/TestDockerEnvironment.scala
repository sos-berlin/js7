package js7.tests.docker

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO, Resource, ResourceIO}
import cats.syntax.all.*
import java.nio.file.Files.{createDirectories, createDirectory, exists, setPosixFilePermissions}
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import js7.agent.TestAgent
import js7.base.catsutils.OurIORuntime
import js7.base.configutils.Configs
import js7.base.configutils.Configs.{HoconStringInterpolator, configMonoid}
import js7.base.generic.SecretString
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{deleteDirectoryContentRecursively, deleteDirectoryRecursively, temporaryDirectory}
import js7.base.log.Logger
import js7.base.service.MainService
import js7.base.utils.CatsUtils.combine
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.Js7Configuration
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.RunningController
import js7.controller.configuration.ControllerConfiguration
import js7.data.agent.AgentPath
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.provider.Provider
import js7.provider.configuration.ProviderConfiguration
import js7.service.pgp.PgpCommons.{RichPGPPublicKey, toArmoredAsciiBytes}
import js7.service.pgp.PgpSigner
import js7.tests.testenv.DirectorEnv
import scala.util.chaining.scalaUtilChainingOps

// TODO
//  • Switch from loopback-is-public to HTTPS authentication
//  • ? SubagentEnv provides port and HTTPS certificate with private key
//  • ? ControllerEnv too
final class TestDockerEnvironment private(
  providerResource: ResourceIO[Provider],
  controllerResource: ResourceIO[RunningController],
  subagentsResource: ResourceIO[List[MainService]]):

  def run: IO[ExitCode] =
    servicesResource.use: services =>
      services.parTraverse:
        _.untilTerminated.map(_.toExitCode)
    .map:
      _.find(_ != ExitCode.Success) getOrElse ExitCode.Success

  private def servicesResource: ResourceIO[List[MainService]] =
    for
      // Start bottom-up (and stop top-down)
      subagents <- subagentsResource
      controller <- controllerResource
      provider <- providerResource
    yield
      provider :: controller :: subagents


object TestDockerEnvironment:

  private val logger = Logger[this.type]
  private val controllerPort = 4444

  private val providerResourcePaths = Seq(
    "provider/config/private/private.conf",
    "provider/config/live/agent-1.agent.json",
    "provider/config/live/agent-2.agent.json",
    "provider/config/live/subagent-1a.subagent.json",
    "provider/config/live/subagent-1b.subagent.json",
    "provider/config/live/subagent-1c.subagent.json",
    "provider/config/live/subagent-2a.subagent.json",
    "provider/config/live/1GB-stdout.workflow.txt",
    "provider/config/live/10MB-stdout.workflow.json",
    "provider/config/live/child-processes.workflow.json",
    "provider/config/live/ten-processes.workflow.txt",
    "provider/config/live/test.workflow.txt",
    "provider/config/live/testCase3.workflow.json",
    "provider/config/live/testCase3A.workflow.json",
    "provider/config/live/testCase3Empty.workflow.json",
    "provider/config/live/one-process.workflow.txt",
    "provider/config/live/jocDailyPlan.calendar.json",
    "provider/config/live/მაგალითად.workflow.json",
    "provider/config/order-generators/test.order.xml",
    "provider/config/order-generators/поръчка.order.xml")

  private val agentsAndSubagents: Map[AgentPath, List[SubagentId]] =
    Map(
      AgentPath("agent-1") -> List(
        SubagentId("subagent-1a"),
        SubagentId("subagent-1b"),
        SubagentId("subagent-1c")),
      AgentPath("agent-2") -> List(
        SubagentId("subagent-2a")))

  private val subagentIsClusterBackup = Set(SubagentId("subagent-1b"))
  private val agentPaths = agentsAndSubagents.keys.toList
  private val subagentIds = agentsAndSubagents.flatMap(_._2).toList

  def temporaryResource(dontCleanUp: Boolean = false): ResourceIO[TestDockerEnvironment] =
    Resource.suspend:
      IO.blocking:
        val directory =
          (temporaryDirectory / "TestDockerExample").tap: directory =>
            println(s"Using directory $directory")
        if !Files.exists(directory) then
          createDirectory(directory)
        else
          println(s"Deleting $directory")
          deleteDirectoryContentRecursively(directory)
        resource(directory, dontCleanUp)

  def resource(directory: Path, dontCleanUp: Boolean = false): ResourceIO[TestDockerEnvironment] =
    for
      _ <- directoryResource(directory, dontCleanUp)
      env <- Resource.eval(IO(buildTestDockerEnvironment(directory)))
    yield
      env

  private def directoryResource(directory: Path, dontCleanUp: Boolean = false): ResourceIO[Unit] =
    Resource(IO.blocking:
      if exists(directory) then
        logger.warn(s"Deleting $directory")
        deleteDirectoryContentRecursively(directory)
      () -> IO.blocking:
        if !dontCleanUp then
          deleteDirectoryRecursively(directory))

  private def buildTestDockerEnvironment(directory: Path): TestDockerEnvironment =
    val (signer, verifier) = PgpSigner.forTest(SecretString("PGP-PASSWORD"))
    val httpsDir = directory / "https"
    val controllerDir = directory / "controller"
    val subagentsDir = directory / "subagents"

    val subagentToItem: Map[SubagentId, SubagentItem] =
      agentsAndSubagents.toList.flatMap: (agentPath, subagentIds) =>
        subagentIds.map: subagentId =>
          val port = findFreeTcpPort()
          SubagentItem(subagentId, agentPath, Uri(s"http://localhost:$port"))
      .toKeyedMap(_.id)

    val subagentIds = subagentToItem.keys.toList

    //lazy val openssl =
    //  createDirectory(httpsDir)
    //  Openssl(httpsDir)
    //
    //lazy val providerCert: Openssl.CertWithPrivateKey =
    //  openssl.generateCertWithPrivateKey("provider", "/CN=Provider").orThrow
    //
    //lazy val controllerCert: Openssl.CertWithPrivateKey =
    //  openssl.generateCertWithPrivateKey("controller", "/CN=Controller").orThrow

    def providerResource: ResourceIO[Provider] =
      val controllerUri = s"http://localhost:$controllerPort"
      for
        given IORuntime <- OurIORuntime.resource[IO]("Provider", Js7Configuration.defaultConfig)
        _ <- providerEnv
        provider <- Provider.resource:
          ProviderConfiguration.fromCommandLine(
            CommandLineArguments(Seq(
              s"--config-directory=$directory/provider/config",
              s"--controller-uri=$controllerUri")),
            config"""
              js7.provider.add-orders-every = 10s
              js7.provider.add-orders-earlier = 1s
              """)
      yield
        provider

    def providerEnv: ResourceIO[Path] =
      Resource(IO.blocking:
        val dir = directory / "provider"
        createDirectory(dir)
        createDirectories(dir / "config/private")
        providerResourcePaths.foreach(provideDockerExampleFile)

        for subagentId <- subagentIds do
          val file = dir / s"config/live/${subagentId.string}.subagent.json"
          file := file.contentString.replace(
            s"https://js7-${subagentId.string}:4443",
            subagentToItem(subagentId).uri.string)

        //dir / "config/private/https-keystore.p12" := providerCert.p12File
        dir / "config/private/private-pgp-key.asc" := signer.pgpSecretKey.toArmoredAsciiBytes
        dir -> IO(deleteDirectoryContentRecursively(dir)))

    def controllerResource: ResourceIO[RunningController] =
      for
        controllerConf <- Resource.eval(IO.blocking:
          createDirectories(controllerDir / "config/private")
          createDirectories(controllerDir / "data")
          provideDockerExampleFile("controller/config/private/private.conf")
          createDirectory(controllerDir / "config/private/trusted-pgp-keys")
          controllerDir / "config/private/trusted-pgp-keys" / "pgp.asc" :=
            signer.pgpSecretKey.getPublicKey.toArmoredAsciiBytes
          //controllerDir / "config/private/https-keystore.p12" := controllerCert.p12File
          ControllerConfiguration.forTest(
            configAndData = controllerDir,
            httpPort = Some(controllerPort),
            name = "Controller",
            config = config"js7.web.server.auth.loopback-is-public = on"))
        given IORuntime <- RunningController.ioRuntimeResource[IO](controllerConf)
        controller <- RunningController.resource(controllerConf)
      yield
        controller

    def subagentsResource: ResourceIO[List[TestAgent]] =
      for
        _ <- Resource.eval(IO.blocking:
          createDirectory(directory / "subagents"))
        subagents <- subagentIds.parTraverse(subagentResource)
      yield
        subagents

    def subagentResource(subagentId: SubagentId): ResourceIO[TestAgent] =
      val uri = Uri(s"http://localhost:${subagentToItem(subagentId)}")

      Resource.suspend(IO: // Because DirectorEnv constructor creates files
        DirectorEnv(
          subagentToItem(subagentId),
          rootDirectory = directory,
          name = subagentId.string,
          verifier = verifier,
          extraConfig = combine(
            Configs.loadResource(javaResource(s"${subagentId.string}/config/agent.conf")),
            Configs.loadSecretResource(javaResource(s"${subagentId.string}/config/private/private.conf")),
            config"js7.web.server.auth.loopback-is-public = true"),
          isClusterBackup = subagentIsClusterBackup(subagentId)
        ).testAgentResource)
      .flatTap: _ =>
        Resource.eval(IO:
          provideSubagentFiles(subagentId))

    def provideSubagentFiles(subagentId: SubagentId): Unit =
      val subagent = subagentId.string
      provideDockerExampleFile(s"$subagent/config/executables/fail")
      provideDockerExampleFile(s"$subagent/config/executables/sleep")
      provideDockerExampleFile(s"$subagent/config/executables/test")

      //private_ / "https-keystore.p12" :=
      //  openssl.generateCertWithPrivateKey(subagent, s"/CN=$subagent").orThrow.p12File

    def provideDockerExampleFile(path: String) =
      val resource = javaResource(path)
      if !resource.exists then
        logger.warn(s"Missing JavaResource $path")
      else
        val dir = if path.startsWith("subagent") then subagentsDir else directory
        createDirectories((dir / path).getParent)
        resource.copyToFile(dir / path)
        if path.contains("/executables/") then
          setPosixFilePermissions(dir / path, PosixFilePermissions.fromString("rwx------"))

    new TestDockerEnvironment(providerResource, controllerResource, subagentsResource)

  end buildTestDockerEnvironment

  private def javaResource(path: String): JavaResource =
    JavaResource(s"js7/install/docker/volumes/$path")
