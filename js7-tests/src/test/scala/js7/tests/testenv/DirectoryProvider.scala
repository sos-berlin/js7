package js7.tests.testenv

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource, ResourceIO}
import cats.syntax.parallel.*
import com.typesafe.config.{Config, ConfigFactory}
import fs2.Stream
import java.nio.file.Files.{createDirectories, createDirectory, createTempDirectory}
import java.nio.file.{Files, Path}
import js7.agent.{RunningAgent, TestAgent}
import js7.base.auth.Admission
import js7.base.catsutils.CatsEffectExtensions.orIfNone
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.crypt.{DocumentSigner, SignatureVerifier, Signed, SignedString}
import js7.base.fs2utils.StreamExtensions.+:
import js7.base.generic.SecretString
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.https.{HttpsConfig, KeyStoreRef, TrustStoreRef}
import js7.base.log.{CorrelId, Logger}
import js7.base.problem.Checked.*
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.closeOnError
import js7.base.utils.CatsBlocking.*
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.Closer.syntax.{RichClosersAny, RichClosersAutoCloseable}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, Atomic, HasCloser}
import js7.base.web.Uri
import js7.cluster.watch.ClusterWatch.OnUndecidableClusterNodeLoss
import js7.cluster.watch.ClusterWatchService
import js7.common.configuration.Js7Configuration
import js7.common.pekkoutils.Pekkos
import js7.common.utils.Exceptions.repeatUntilNoException
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.RunningController
import js7.controller.RunningController.TestWiring
import js7.controller.client.PekkoHttpControllerApi
import js7.controller.client.PekkoHttpControllerApi.admissionsToApiResource
import js7.controller.configuration.ControllerConfiguration
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.cluster.ClusterWatchId
import js7.data.controller.ControllerState.signableItemJsonCodec
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddVersion, RemoveVersioned}
import js7.data.item.{InventoryItem, ItemOperation, ItemSigner, SignableItem, SignableSimpleItem, UnsignedSimpleItem, VersionId, VersionedItem, VersionedItemPath}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.proxy.ControllerApi
import js7.service.pgp.PgpSigner
import js7.subagent.Subagent
import js7.tests.testenv.DirectoryProvider.*
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.{Iterable, Map}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
@TestOnly
final class DirectoryProvider(
  controllerPort: => Int = findFreeTcpPort(),
  agentPaths: Seq[AgentPath],
  bareSubagentItems: Seq[SubagentItem] = Nil,
  items: Seq[InventoryItem] = Nil,
  controllerConfig: Config = ConfigFactory.empty,
  controllerTestWiring: RunningController.TestWiring = RunningController.TestWiring.empty,
  agentHttps: Boolean = false,
  agentHttpsMutual: Boolean = false,
  agentConfig: Config = ConfigFactory.empty,
  agentPorts: Seq[Int] = Nil,
  directorEnvToAgentRef: DirectorEnv => AgentRef =
    a => AgentRef(a.agentPath, Vector(a.localSubagentId)),
  isBackup: Boolean = false,
  primarySubagentsDisabled: Boolean = false,
  provideAgentHttpsCertificate: Boolean = false,
  provideAgentClientCertificate: Boolean = false,
  controllerKeyStore: Option[JavaResource] = Some(ControllerKeyStoreResource),
  controllerTrustStores: Iterable[JavaResource] = Nil,
  signer: DocumentSigner = defaultSigner,
  val verifier: SignatureVerifier = defaultVerifier,
  testName: Option[String] = None,
  useDirectory: Option[Path] = None,
  doNotAddItems: Boolean = false,
  commonIORuntime: Option[IORuntime] = None)
extends HasCloser:
  private lazy val controllerPort_ = controllerPort

  val directory = useDirectory.getOrElse:
    createTempDirectory(testName.fold("test-")(_ + "-"))
      .withCloser: dir =>
        repeatUntilNoException(10.s, 10.ms):  // Windows
          deleteDirectoryRecursively(dir)

  val controllerEnv = new ControllerEnv(directory / "controller",
    verifier = verifier,
    keyStore = controllerKeyStore,
    trustStores = controllerTrustStores,
    agentHttpsMutual = agentHttpsMutual
  ).closeWithCloser

  private val tmpDirCounter = Atomic(0)

  createDirectory(directory / "subagents")

  val agentToEnv: Map[AgentPath, DirectorEnv] =
    agentPaths
      .zip(agentPorts ++ Seq.fill(agentPaths.length - agentPorts.length)(findFreeTcpPort()))
      .map: (agentPath, port) =>
        val localSubagentId = toLocalSubagentId(agentPath, isBackup = isBackup)
        val localhost = if agentHttps then "https://localhost" else "http://127.0.0.1"

        val localSubagentItem = SubagentItem(
          localSubagentId, agentPath, disabled = primarySubagentsDisabled,
          uri = Uri(s"$localhost:$port"))

        agentPath -> newDirectorEnv(
          localSubagentItem,
          otherSubagentIds = bareSubagentItems
            .collect { case o: SubagentItem if o.agentPath == agentPath => o.id })
      .toMap

  val agentEnvs: Vector[DirectorEnv] = agentToEnv.values.toVector
  lazy val agentRefs: Vector[AgentRef] = agentEnvs.map(directorEnvToAgentRef)
  lazy val subagentItems: Vector[SubagentItem] = agentEnvs.map(_.subagentItem) ++ bareSubagentItems
  lazy val subagentId: SubagentId = agentEnvs.head.localSubagentId

  private val itemsHaveBeenAdded = Atomic(false)

  closeOnError(this):
    agentEnvs foreach prepareAgentFiles

    items
      .collect { case o: SubagentItem => o }
      .map(_.agentPath)
      .distinct
      .foreach: agentPath =>
        controllerEnv.writeAgentAuthentication(
          agentPath,
          SecretString(s"$agentPath-PASSWORD")/*FIXME Duplicate in DirectorEnv*/)

  val itemSigner = new ItemSigner(signer, signableItemJsonCodec)

  def toSignedString[A <: SignableItem](item: A): SignedString =
    itemSigner.toSignedString(item)

  def sign[A <: SignableItem](item: A): Signed[A] =
    itemSigner.sign(item)

  /** Proxy's ControllerApi */
  def controllerApiResource(runningController: RunningController): ResourceIO[ControllerApi] =
    ControllerApi.resource(
      admissionsToApiResource(
        Nel.one(controllerAdmission(runningController))
      )(using runningController.actorSystem))

  def controllerAdmission(runningController: RunningController): Admission =
    Admission(runningController.localUri, Some(controllerEnv.userAndPassword))

  def run[A](body: (TestController, IndexedSeq[TestAgent]) => A)(using IORuntime): A =
    runAgents()(agents =>
      runController()(controller =>
        body(controller, agents)))

  def runController[A](
    httpPort: Option[Int] = Some(controllerPort_),
    dontWaitUntilReady: Boolean = false,
    config: Config = ConfigFactory.empty)
    (body: TestController => A)
    (using IORuntime)
  : A =
    testControllerResource(httpPort = httpPort, config = config)
      .blockingUse(99.s): testController =>
        val result =
          try
            if !dontWaitUntilReady then
              testController.waitUntilReady()
            body(testController)
          catch case NonFatal(t) =>
            // Pekko may crash before the caller gets the error so we log the error here
            logger.error(s"💥💥💥 ${t.toStringWithCauses}", t.nullIfNoStackTrace)
            try testController.terminate().await(99.s)
            catch case t2: Throwable if t2 ne t =>
              t.addSuppressed(t2)
            throw t
        try testController.stop.await(99.s)
        catch case NonFatal(t) =>
          // Pekko may crash before the caller gets the error so we log the error here
          logger.error(s"💥💥💥 ${t.toStringWithCauses}", t.nullIfNoStackTrace)
          try testController.stop.await(99.s)
          catch case t2: Throwable if t2 ne t =>
            t.addSuppressed(t2)
          throw t
        result

  def newController(
    testWiring: TestWiring = controllerTestWiring,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(controllerPort_),
    httpsPort: Option[Int] = None)
    (using IORuntime)
  : TestController =
    testControllerResource(testWiring, config, httpPort, httpsPort)
      .allocated
      .map(_._1)
      .await(99.s)

  def testControllerResource(
    testWiring: TestWiring = controllerTestWiring,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(controllerPort_),
    httpsPort: Option[Int] = None)
  : ResourceIO[TestController] =
    Resource.make(
      runningControllerResource(testWiring, config, httpPort, httpsPort)
        .toAllocated
        .map(runningController =>
          new TestController(
            runningController,
            controllerAdmission(runningController.allocatedThing))))(
      release = _.stop)

  private def runningControllerResource(
    testWiring: RunningController.TestWiring = controllerTestWiring,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(controllerPort_),
    httpsPort: Option[Int] = None)
  : ResourceIO[RunningController] =
    val conf = ControllerConfiguration.forTest(
      configAndData = controllerEnv.directory,
      config.withFallback(controllerConfig).withFallback(TestConfig),
      httpPort = httpPort,
      httpsPort = httpsPort,
      name = controllerName)

    def startForTest(runningController: RunningController)(using ioRuntime: IORuntime): Unit =
      if !doNotAddItems && !isBackup && (agentRefs.nonEmpty || items.nonEmpty) then
        if !itemsHaveBeenAdded.getAndSet(true) then
          runningController.waitUntilReady()
          runningController.updateUnsignedSimpleItemsAsSystemUser(agentRefs ++ subagentItems)
            .await(99.s).orThrow

          if items.nonEmpty then
            val versionedItems = items.collect { case o: VersionedItem => o }.map(_.withVersion(Vinitial))
            val signableItems = versionedItems ++ items.collect { case o: SignableSimpleItem => o }
            runningController
              .updateItemsAsSystemUser(
                Stream.iterable(items).collect { case o: UnsignedSimpleItem => ItemOperation.AddOrChangeSimple(o) } ++
                  Stream.iterable(versionedItems.nonEmpty ? ItemOperation.AddVersion(Vinitial)) ++
                  Stream.iterable(signableItems)
                    .map(itemSigner.toSignedString)
                    .map(ItemOperation.AddOrChangeSigned.apply))
              .await(99.s).orThrow
      given ExecutionContext = ioRuntime.compute
      for t <- runningController.terminated.failed do
        logger.error(s"💥💥💥 ${t.toStringWithCauses}", t.nullIfNoStackTrace)
        logger.debug(t.toStringWithCauses, t)

    CorrelId.bindNew:
      Resource.eval(IO.pure(commonIORuntime))
        .orIfNone:
          RunningController.ioRuntimeResource[IO](conf)
        .flatMap(implicit ioRuntime =>
          RunningController
            .resource(conf, testWiring)(using ioRuntime)
            .evalTap: runningController =>
              IO(startForTest(runningController)))

  def runAgents[A](
    agentPaths: Seq[AgentPath] = DirectoryProvider.this.agentPaths,
    suppressSnapshot: Boolean = false)(
    body: Vector[TestAgent] => A)
    (using IORuntime)
  : A =
    val agentAllocatedSeq = agentEnvs
      .filter(o => agentPaths.contains(o.agentPath))
      .map(_.testAgentResource)
      .parTraverse:
        _.toAllocated
      .await(99.s)
    val result =
      try body(agentAllocatedSeq.map(_.allocatedThing))
      catch case NonFatal(t) =>
        // Pekko may crash before the caller gets the error, so we log the error here
        logger.error(s"💥💥💥 ${t.toStringWithCauses}", t.nullIfNoStackTrace)
        try agentAllocatedSeq.parTraverse(_.release).await(99.s)
        catch case t2: Throwable if t2 ne t =>
          t.addSuppressed(t2)
        throw t

    agentAllocatedSeq.parTraverse: allo =>
      allo.allocatedThing.terminate(suppressSnapshot = suppressSnapshot) *> allo.release
    .await(99.s)
    result

  def startAgents(testWiring: RunningAgent.TestWiring = RunningAgent.TestWiring.empty)
  : IO[Seq[TestAgent]] =
    agentEnvs.parTraverse(a => startAgent(a.agentPath, testWiring))

  def startAgent(
    agentPath: AgentPath,
    testWiring: RunningAgent.TestWiring = RunningAgent.TestWiring.empty)
  : IO[TestAgent] =
    TestAgent.start(
      agentToEnv(agentPath).agentConf,
      testWiring)

  def startBareSubagents(): IO[Map[SubagentId, Allocated[IO, Subagent]]] =
    bareSubagentItems
      .parTraverse(subagentItem =>
        bareSubagentResource(subagentItem, config = agentConfig.withFallback(TestConfig))
          .toAllocated
          .map(subagentItem.id -> _))
      .map(_.toMap)

  def updateVersionedItems(
    controller: TestController,
    versionId: VersionId,
    change: Seq[VersionedItem] = Nil,
    delete: Seq[VersionedItemPath] = Nil)
    (using IORuntime)
  : Unit =
    controller.updateItemsAsSystemUser(
      AddVersion(versionId) +:
        (Stream.iterable(change)
          .map(_.withVersion(versionId))
          .map(itemSigner.toSignedString)
          .map(AddOrChangeSigned(_)) ++
          Stream.iterable(delete)
            .map(RemoveVersioned.apply))
    ).await(99.s).orThrow

  private def controllerName = testName.fold(ControllerConfiguration.DefaultName)(_ + "-Controller")

  def prepareAgentFiles(env: DirectorEnv): Unit =
    //env.createDirectoriesAndFiles()
    controllerEnv.writeAgentAuthentication(env)

  def directorEnvResource(
    subagentItem: SubagentItem,
    suffix: String = "",
    otherSubagentIds: Seq[SubagentId] = Nil,
    isClusterBackup: Boolean = false,
    suppressSignatureKeys: Boolean = false,
    extraConfig: Config = ConfigFactory.empty)
  : ResourceIO[DirectorEnv] =
    Resource
      .fromAutoCloseable(IO(
        newDirectorEnv(subagentItem, suffix, otherSubagentIds,
          isClusterBackup = isClusterBackup,
          suppressSignatureKeys = suppressSignatureKeys,
          extraConfig = extraConfig)))

  private def newDirectorEnv(
    subagentItem: SubagentItem,
    suffix: String = "",
    otherSubagentIds: Seq[SubagentId] = Nil,
    isClusterBackup: Boolean = false,
    suppressSignatureKeys: Boolean = false,
    extraConfig: Config = ConfigFactory.empty)
  : DirectorEnv =
    new DirectorEnv(
      subagentItem = subagentItem,
      rootDirectory = directory,
      name = subagentName(subagentItem.id, suffix = suffix),
      verifier = verifier,
      mutualHttps = agentHttpsMutual,
      provideHttpsCertificate = provideAgentHttpsCertificate,
      provideClientCertificate = provideAgentClientCertificate,
      isClusterBackup = isClusterBackup,
      suppressSignatureKeys = suppressSignatureKeys,
      otherSubagentIds = otherSubagentIds,
      extraConfig = extraConfig.withFallback(agentConfig).withFallback(TestConfig))

  def bareSubagentResource(
    subagentItem: SubagentItem,
    director: SubagentId = toLocalSubagentId(agentPaths.head),
    config: Config = ConfigFactory.empty,
    suffix: String = "",
    suppressSignatureKeys: Boolean = false,
    testWiring: Subagent.TestWiring = Subagent.TestWiring.empty)
  : ResourceIO[Subagent] =
    for
      env <- bareSubagentEnvResource(subagentItem,
        director = director,
        suffix = suffix,
        suppressSignatureKeys = suppressSignatureKeys,
        extraConfig = config)
      subagent <- env.subagentResource(testWiring.envResources)
    yield
      subagent

  private def bareSubagentEnvResource(
    subagentItem: SubagentItem,
    director: SubagentId,
    suffix: String = "",
    suppressSignatureKeys: Boolean = false,
    extraConfig: Config = ConfigFactory.empty)
  : ResourceIO[BareSubagentEnv] =
    Resource.fromAutoCloseable(IO:
      new BareSubagentEnv(
        subagentItem = subagentItem,
        directorSubagentId = director,
        name = subagentName(subagentItem.id, suffix = suffix),
        rootDirectory = directory,
        verifier = verifier,
        mutualHttps = agentHttpsMutual,
        provideHttpsCertificate = provideAgentHttpsCertificate,
        provideClientCertificate = provideAgentClientCertificate,
        suppressSignatureKeys = suppressSignatureKeys,
        extraConfig = extraConfig.withFallback(agentConfig).withFallback(TestConfig)))

  def bareSubagentToDirectory(subagentId: SubagentId, suffix: String = ""): Path =
    directory / "subagents" / subagentName(subagentId, suffix)

  def subagentName(subagentId: SubagentId, suffix: String = ""): String =
    testName.fold("")(_ + "-") + subagentId.string + suffix

  def withTemporaryDirectory[A](body: Path => A): A =
    val nr = tmpDirCounter.incrementAndGet()
    val dir = directory / s"tmp-$nr"
    createDirectories(dir)
    try body(dir)
    finally deleteDirectoryRecursively(dir)


object DirectoryProvider:
  private val Vinitial = VersionId("INITIAL")
  private val logger = Logger[this.type]

  // TODO Maybe change js7.conf?
  private val TestConfig = config"""
    # Accelerate tests
    # Very short keep-alive (heartbeat), because a connection close is only detected when
    # a message (keep-alive) arrives
    js7.web.client.keep-alive = 100ms
    js7.web.server.delay-shutdown = 0s
    js7.web.server.shutdown-delay = 100ms
    js7.web.server.shutdown-timeout = 100ms
  """

  def toLocalSubagentId(agentPath: AgentPath, isBackup: Boolean = false): SubagentId =
    SubagentId(agentPath.string + (if !isBackup then "-0" else "-1"))

  final val StdoutOutput = if isWindows then "TEST\r\n" else "TEST ☘\n"

  final def script(duration: FiniteDuration, resultVariable: Option[String] = None): String =
    if isWindows then
      (s"""@echo off
          |echo ${StdoutOutput.trim}
          |ping -n ${1 + (duration + 999999.µs).toMillis / 1000} 127.0.0.1 >nul""" +
          resultVariable.fold("")(o => s"""|echo result=SCRIPT-VARIABLE-%SCHEDULER_PARAM_${o.toUpperCase}% >>"%SCHEDULER_RETURN_VALUES%"""")
      ).stripMargin
    else
      (s"""echo ${StdoutOutput.trim}
          |sleep ${duration.toDecimalString}""" +
          resultVariable.fold("")(o => s"""|echo "result=SCRIPT-VARIABLE-$$SCHEDULER_PARAM_${o.toUpperCase}" >>"$$SCHEDULER_RETURN_VALUES"""")
      ).stripMargin

  final def waitingForFileScript(file: Path, delete: Boolean = false): String =
    if isWindows then
       s"""@echo off
          |:LOOP
          |  if exist "$file" goto FOUND
          |  ping -n 2 127.0.0.1 >nul
          |  goto LOOP
          |:FOUND
          |""".stripMargin + (delete ?? s"del $file\n")
    else
       s"""#!/usr/bin/env bash
          |set -euo pipefail
          |while [ ! -e '$file' ]; do
          |  sleep 0.1
          |done
          |""".stripMargin + (delete ?? s"rm '$file'\n")

  /* Following resources have been generated with the command line:
     js7-common/src/main/resources/js7/common/pekkohttp/https/generate-self-signed-ssl-certificate-test-keystore.sh \
        --distinguished-name="CN=Primary Controller, DC=primary-controller, DC=DirectoryProvider, DC=tests, DC=js7, DC=sh" \
        --alias=controller \
        --host=localhost \
        --config-directory=js7-tests/src/test/resources/js7/tests/controller/config
   */
  private[testenv] val ControllerKeyStoreResource = JavaResource("js7/tests/controller/config/private/https-keystore.p12")
  private[testenv] lazy val controllerClientKeyStoreRef = KeyStoreRef(
    ControllerKeyStoreResource.url,
    alias = None,
    SecretString("jobscheduler"),
    SecretString("jobscheduler"))
  val ExportedControllerTrustStoreResource = JavaResource("js7/tests/controller/config/export/https-truststore.p12")
  lazy val ExportedControllerTrustStoreRef = TrustStoreRef(
    ExportedControllerTrustStoreResource.url,
    SecretString("jobscheduler"))
    Seq(TrustStoreRef(
      ExportedControllerTrustStoreRef.url,
      SecretString("jobscheduler")))
  lazy val controllerClientHttpsConfig = HttpsConfig(
    Some(controllerClientKeyStoreRef),
    Seq(ExportedControllerTrustStoreRef))

  /* Following resources have been generated with the command line:
     js7-common/src/main/resources/js7/common/pekkohttp/https/generate-self-signed-ssl-certificate-test-keystore.sh \
        --distinguished-name="CN=Agent, DC=agent, DC=DirectoryProvider, DC=tests, DC=js7, DC=sh" \
        --alias=agent \
        --host=localhost \
        --config-directory=js7-tests/src/test/resources/js7/tests/agent/config
   */
  private[testenv] val AgentKeyStoreResource   = JavaResource(
    "js7/tests/agent/config/private/https-keystore.p12")
  private[testenv] val AgentTrustStoreResource = JavaResource(
    "js7/tests/agent/config/export/https-truststore.p12")

  final lazy val (defaultSigner, defaultVerifier) = PgpSigner.forTest()

  def clusterWatchServiceResource(
    clusterWatchId: ClusterWatchId,
    admissions: Nel[Admission],
    httpsConfig: HttpsConfig,
    config: Config = ConfigFactory.empty,
    onUndecidableClusterNodeLoss: OnUndecidableClusterNodeLoss = _ => IO.unit)
  : ResourceIO[ClusterWatchService] =
    Pekkos
      .actorSystemResource(clusterWatchId.string)
      .flatMap(implicit actorSystem =>
        ClusterWatchService.resource(
          clusterWatchId,
          admissions.traverse(PekkoHttpControllerApi.resource(_, httpsConfig)),
          config.withFallback(Js7Configuration.defaultConfig),
          onUndecidableClusterNodeLoss = onUndecidableClusterNodeLoss))
