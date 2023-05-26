package js7.tests.testenv

import cats.effect.Resource
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Files.{createDirectory, createTempDirectory}
import java.nio.file.Path
import js7.agent.{RunningAgent, TestAgent}
import js7.base.auth.Admission
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.crypt.{DocumentSigner, SignatureVerifier, Signed, SignedString}
import js7.base.eventbus.StandardEventBus
import js7.base.generic.SecretString
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.https.{HttpsConfig, KeyStoreRef, TrustStoreRef}
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.RichMonixResource
import js7.base.problem.Checked.*
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.IOExecutor
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.closeOnError
import js7.base.utils.CatsBlocking.BlockingTaskResource
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.Closer.syntax.RichClosersAny
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, HasCloser}
import js7.base.web.Uri
import js7.cluster.watch.ClusterWatchService
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.akkautils.Akkas
import js7.common.configuration.Js7Configuration
import js7.common.system.ThreadPools
import js7.common.utils.Exceptions.repeatUntilNoException
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.RunningController
import js7.controller.RunningController.TestWiring
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResource
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
import js7.subagent.configuration.SubagentConf
import js7.tests.testenv.DirectoryProvider.*
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.traced
import monix.execution.atomic.AtomicBoolean
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.{Iterable, Map}
import scala.concurrent.duration.*
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
@TestOnly
final class DirectoryProvider(
  agentPaths: Seq[AgentPath],
  agentToBareSubagent: Map[AgentPath, Seq[SubagentId]] = Map.empty,
  items: Seq[InventoryItem] = Nil,
  controllerConfig: Config = ConfigFactory.empty,
  controllerTestWiring: RunningController.TestWiring = RunningController.TestWiring.empty,
  agentHttps: Boolean = false,
  agentHttpsMutual: Boolean = false,
  agentConfig: Config = ConfigFactory.empty,
  agentPorts: Seq[Int] = Nil,
  isBackup: Boolean = false,
  subagentsDisabled: Boolean = false,
  provideAgentHttpsCertificate: Boolean = false,
  provideAgentClientCertificate: Boolean = false,
  controllerKeyStore: Option[JavaResource] = Some(ControllerKeyStoreResource),
  controllerTrustStores: Iterable[JavaResource] = Nil,
  signer: DocumentSigner = defaultSigner,
  val verifier: SignatureVerifier = defaultVerifier,
  testName: Option[String] = None,
  useDirectory: Option[Path] = None,
  doNotAddItems: Boolean = false,
  scheduler: Option[Scheduler] = None)
extends HasCloser
{
  coupleScribeWithSlf4j()

  val directory = useDirectory.getOrElse(
    createTempDirectory(testName.fold("test-")(_ + "-"))
      .withCloser { dir =>
        repeatUntilNoException(10.s, 10.ms) {  // Windows
          deleteDirectoryRecursively(dir)
        }
      })

  val controllerEnv = new ControllerEnv(directory / "controller",
    verifier = verifier,
    keyStore = controllerKeyStore,
    trustStores = controllerTrustStores,
    agentHttpsMutual = agentHttpsMutual)

  createDirectory(directory / "subagents")

  val agentToEnv: Map[AgentPath, AgentEnv] =
    agentPaths
      .zip(agentPorts ++ Seq.fill(agentPaths.length - agentPorts.length)(findFreeTcpPort()))
      .map { case (agentPath, port) =>
        val localSubagentId = toLocalSubagentId(agentPath, isBackup = isBackup)
        val localhost = if (agentHttps) "https://localhost" else "http://127.0.0.1"

        val localSubagentItem = SubagentItem(
          localSubagentId, agentPath, disabled = subagentsDisabled,
          uri = Uri(s"$localhost:$port"))
        // TODO backup Subagent
        //val directorSubagentItems = for (port <- Nel.of(primaryPort, maybeBackupPort)) yield
        //  SubagentItem(
        //    localSubagentId, agentPath, disabled = subagentsDisabled,
        //    uri = Uri(s"$localhost:$port"))

        agentPath -> newAgentEnv(localSubagentItem)
      }
      .toMap

  val agentEnvs: Vector[AgentEnv] = agentToEnv.values.toVector
  lazy val agentRefs: Vector[AgentRef] =
    for (a <- agentEnvs) yield AgentRef(a.agentPath, Seq(a.localSubagentId))
  lazy val subagentItems: Vector[SubagentItem] = agentEnvs.flatMap(_.subagentItems)
  lazy val subagentId: SubagentId = agentEnvs.head.localSubagentId

  private val itemsHaveBeenAdded = AtomicBoolean(false)

  closeOnError(this) {
    controllerEnv.createDirectoriesAndFiles()
    agentEnvs foreach prepareAgentFiles

    items
      .collect { case o: SubagentItem => o }
      .map(_.agentPath)
      .distinct
      .foreach { agentPath =>
        controllerEnv.writeAgentAuthentication(
          agentPath,
          SecretString(s"$agentPath-PASSWORD")/*FIXME Duplicate in AgentEnv*/)
      }
  }

  val itemSigner = new ItemSigner(signer, signableItemJsonCodec)

  def toSignedString[A <: SignableItem](item: A): SignedString =
    itemSigner.toSignedString(item)

  def sign[A <: SignableItem](item: A): Signed[A] =
    itemSigner.sign(item)

  /** Proxy's ControllerApi */
  def controllerApiResource(runningController: RunningController): Resource[Task, ControllerApi] =
    ControllerApi.resource(
      admissionsToApiResource(
        Nel.one(controllerAdmission(runningController)))(
        runningController.actorSystem))

  def controllerAdmission(runningController: RunningController): Admission =
    Admission(runningController.localUri, Some(controllerEnv.userAndPassword))

  def run[A](body: (TestController, IndexedSeq[TestAgent]) => A): A =
    runAgents()(agents =>
      runController()(controller =>
        body(controller, agents)))

  def runController[A](
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    dontWaitUntilReady: Boolean = false,
    config: Config = ConfigFactory.empty)
    (body: TestController => A)
  : A =
    testControllerResource(httpPort = httpPort, config = config)
      .blockingUse(99.s) { testController =>
        val result =
          try {
            if (!dontWaitUntilReady) {
              testController.waitUntilReady()
            }
            body(testController)
          } catch { case NonFatal(t) =>
            // Akka may crash before the caller gets the error so we log the error here
            logger.error(s"💥💥💥 ${t.toStringWithCauses}", t.nullIfNoStackTrace)
            try testController.terminate() await 99.s
            catch { case t2: Throwable if t2 ne t => t.addSuppressed(t2) }
            throw t
          }
        try testController.stop await 99.s
        catch { case NonFatal(t) =>
          // Akka may crash before the caller gets the error so we log the error here
          logger.error(s"💥💥💥 ${t.toStringWithCauses}", t.nullIfNoStackTrace)
          try testController.stop.await(99.s)
          catch { case t2: Throwable if t2 ne t => t.addSuppressed(t2) }
          throw t
        }
        result
      }

  def newController(
    testWiring: TestWiring = controllerTestWiring,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    httpsPort: Option[Int] = None)
  : TestController =
    testControllerResource(testWiring, config, httpPort, httpsPort)
      .allocated
      .map(_._1)
      .await(99.s)

  private def testControllerResource(
    testWiring: TestWiring = controllerTestWiring,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    httpsPort: Option[Int] = None)
  : Resource[Task, TestController] =
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
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    httpsPort: Option[Int] = None)
  : Resource[Task, RunningController] = {
    val conf = ControllerConfiguration.forTest(
      configAndData = controllerEnv.directory,
      config.withFallback(controllerConfig),
      httpPort = httpPort,
      httpsPort = httpsPort,
      name = controllerName)

    def startForTest(runningController: RunningController): Unit = {
      if (!doNotAddItems && !isBackup && (agentRefs.nonEmpty || items.nonEmpty)) {
        if (!itemsHaveBeenAdded.getAndSet(true)) {
          runningController.waitUntilReady()
          runningController.updateUnsignedSimpleItemsAsSystemUser(agentRefs ++ subagentItems)
            .await(99.s).orThrow

          if (items.nonEmpty) {
            val versionedItems = items.collect { case o: VersionedItem => o }.map(_ withVersion Vinitial)
            val signableItems = versionedItems ++ items.collect { case o: SignableSimpleItem => o }
            runningController
              .updateItemsAsSystemUser(
                Observable.from(items).collect { case o: UnsignedSimpleItem => ItemOperation.AddOrChangeSimple(o) } ++
                  Observable.fromIterable(versionedItems.nonEmpty ? ItemOperation.AddVersion(Vinitial)) ++
                  Observable.fromIterable(signableItems)
                    .map(itemSigner.toSignedString)
                    .map(ItemOperation.AddOrChangeSigned.apply))
              .await(99.s).orThrow
          }
        }
      }
      for (t <- runningController.terminated.failed) {
        logger.error(s"💥💥💥 ${t.toStringWithCauses}", t.nullIfNoStackTrace)
        logger.debug(t.toStringWithCauses, t)
      }
    }

    CorrelId.bindNew(
      RunningController.threadPoolResource[Task](conf, orCommon = scheduler)
        .flatMap(js7Scheduler =>
          RunningController
            .resource(conf, testWiring)(js7Scheduler)
            .evalTap(runningController => Task {
              startForTest(runningController)
            })
            .executeOn(js7Scheduler)))
  }

  def runAgents[A](
    agentPaths: Seq[AgentPath] = DirectoryProvider.this.agentPaths)(
    body: Vector[TestAgent] => A)
  : A = {
    val agents = agentEnvs
      .filter(o => agentPaths.contains(o.agentPath))
      .map(_.agentConf)
      .parTraverse(a => TestAgent.start(a))
      .await(99.s)

    val result =
      try body(agents)
      catch { case NonFatal(t) =>
        // Akka may crash before the caller gets the error so we log the error here
        logger.error(s"💥💥💥 ${t.toStringWithCauses}", t.nullIfNoStackTrace)
        try agents.parTraverse(_.stop) await 99.s
        catch { case t2: Throwable if t2 ne t => t.addSuppressed(t2) }
        throw t
      }
      agents.traverse(_.terminate()) await 99.s
    result
  }

  def startAgents(testWiring: RunningAgent.TestWiring = RunningAgent.TestWiring.empty)
  : Task[Seq[TestAgent]] =
    agentEnvs.parTraverse(a => startAgent(a.agentPath, testWiring))

  def startAgent(
    agentPath: AgentPath,
    testWiring: RunningAgent.TestWiring = RunningAgent.TestWiring.empty)
  : Task[TestAgent] =
    TestAgent.start(
      agentToEnv(agentPath).agentConf,
      testWiring)

  def startBareSubagents(): Task[Map[SubagentId, Allocated[Task, Subagent]]] =
    agentEnvs
      .flatMap(a => a.bareSubagentItems.map(_ -> a.agentConf.config))
      .parTraverse { case (subagentItem, config) =>
        subagentResource(subagentItem, config)
          .toAllocated
          .map(subagentItem.id -> _)
      }
      .map(_.toMap)

  def updateVersionedItems(
    controller: TestController,
    versionId: VersionId,
    change: Seq[VersionedItem] = Nil,
    delete: Seq[VersionedItemPath] = Nil)
  : Unit =
    controller.updateItemsAsSystemUser(
      AddVersion(versionId) +:
        (Observable.fromIterable(change)
          .map(_ withVersion versionId)
          .map(itemSigner.toSignedString)
          .map(AddOrChangeSigned(_)) ++
          Observable.fromIterable(delete)
            .map(RemoveVersioned.apply))
    ).await(99.s).orThrow

  private def controllerName = testName.fold(ControllerConfiguration.DefaultName)(_ + "-Controller")

  def prepareAgentFiles(env: AgentEnv): Unit = {
    env.createDirectoriesAndFiles()
    controllerEnv.writeAgentAuthentication(env)
  }

  def directorEnvResource(
    subagentItem: SubagentItem,
    suffix: String = "",
    moreSubagentIds: Seq[SubagentId] = Nil,
    isClusterBackup: Boolean = false,
    suppressSignatureKeys: Boolean = false)
  : Resource[Task, DirectorEnv] =
    Resource
      .make(
        acquire = Task(
          newDirectorEnv(subagentItem, suffix = suffix,
            moreSubagentIds = moreSubagentIds,
            isClusterBackup = isClusterBackup,
            suppressSignatureKeys = suppressSignatureKeys)))(
        release = env => Task(
          env.delete()))
      .evalTap(env => Task(
        env.createDirectoriesAndFiles()))

  def newDirectorEnv(
    subagentItem: SubagentItem,
    moreSubagentIds: Seq[SubagentId] = Nil,
    suffix: String = "",
    isClusterBackup: Boolean = false,
    suppressSignatureKeys: Boolean = false)
  : DirectorEnv =
    new DirectorEnv(
      subagentItem = subagentItem,
      name = subagentName(subagentItem.id, suffix = suffix),
      moreSubagentIds = moreSubagentIds,
      rootDirectory = directory,
      verifier = verifier,
      mutualHttps = agentHttpsMutual,
      provideHttpsCertificate = provideAgentHttpsCertificate,
      provideClientCertificate = provideAgentClientCertificate,
      isClusterBackup = isClusterBackup,
      suppressSignatureKeys = suppressSignatureKeys,
      config = agentConfig)

  @deprecated // Duplicate in AgentEnv ?
  def subagentResource(
    subagentItem: SubagentItem,
    config: Config = ConfigFactory.empty,
    suffix: String = "",
    suppressSignatureKeys: Boolean = false)
  : Resource[Task, Subagent] =
    for {
      agentEnv <- agentEnvResource(subagentItem, suffix = suffix,
        suppressSignatureKeys = suppressSignatureKeys)
      trustedSignatureDir = agentEnv.configDir / "private" /
        verifier.companion.recommendedKeyDirectoryName
      conf = toSubagentConf(
        subagentItem.agentPath,
        agentEnv.directory,
        trustedSignatureDir,
        subagentItem.uri.port.orThrow,
        config,
        name = subagentItem.id.string
      ).finishAndProvideFiles
      iox <- IOExecutor.resource[Task](conf.config, name = conf.name + "-I/O")
      testEventBus <- Resource.eval(Task(new StandardEventBus[Any]))
      subagent <- ThreadPools.ownThreadPoolResource(conf.name, conf.config)(
        scheduler => Subagent.resource(conf, iox, testEventBus).executeOn(scheduler))
    } yield subagent

  def agentEnvResource(
    subagentItem: SubagentItem,
    suffix: String = "",
    isClusterBackup: Boolean = false,
    suppressSignatureKeys: Boolean = false)
  : Resource[Task, AgentEnv] =
    Resource
      .make(
        acquire = Task(
          newAgentEnv(subagentItem, suffix = suffix, isClusterBackup = isClusterBackup,
            suppressSignatureKeys = suppressSignatureKeys)))(
        release = env => Task(
          env.delete()))
      .evalTap(env => Task(
        env.createDirectoriesAndFiles()))

  def newAgentEnv(
    subagentItem: SubagentItem,
    suffix: String = "",
    isClusterBackup: Boolean = false,
    suppressSignatureKeys: Boolean = false)
  : AgentEnv =
    new AgentEnv(
      subagentItem = subagentItem,
      name = subagentName(subagentItem.id, suffix = suffix),
      rootDirectory = directory,
      verifier = verifier,
      mutualHttps = agentHttpsMutual,
      provideHttpsCertificate = provideAgentHttpsCertificate,
      provideClientCertificate = provideAgentClientCertificate,
      bareSubagentIds = agentToBareSubagent.getOrElse(subagentItem.agentPath, Nil),
      subagentsDisabled = subagentsDisabled,
      isClusterBackup = isClusterBackup,
      suppressSignatureKeys = suppressSignatureKeys,
      config = agentConfig)

  def bareSubagentToDirectory(subagentId: SubagentId, suffix: String = ""): Path =
    directory / "subagents" / subagentName(subagentId, suffix)

  def subagentName(subagentId: SubagentId, suffix: String = ""): String =
    testName.fold("")(_ + "-") + subagentId.string + suffix

  @deprecated // Duplicate in SubagentEnv ?
  def toSubagentConf(
    agentPath: AgentPath,
    directory: Path,
    trustedSignatureDir: Path,
    port: Int,
    config: Config = ConfigFactory.empty,
    name: String)
  : SubagentConf =
    SubagentConf.of(
      configDirectory = directory / "config",
      dataDirectory = directory / "data",
      logDirectory = directory / "data" / "logs",
      jobWorkingDirectory = directory,
      Seq(WebServerPort.localhost(port)),
      killScript = None,
      name = testName.fold("")(_ + "-") + name,
      config = config
        .withFallback(config"""
          js7.job.execution.signed-script-injection-allowed = yes
          js7.auth.users.${agentPath.string} {
            permissions: [ AgentDirector ]
            password: "plain:AGENT-PASSWORD"
          }
          js7.configuration.trusted-signature-keys {
            ${verifier.companion.typeName} = "$trustedSignatureDir"
          }
          """)
        .withFallback(SubagentConf.DefaultConfig))
}

object DirectoryProvider
{
  private val Vinitial = VersionId("INITIAL")
  private val logger = Logger[this.type]

  def toLocalSubagentId(agentPath: AgentPath, isBackup: Boolean = false): SubagentId =
    SubagentId(agentPath.string + (if (!isBackup) "-0" else "-1"))


  final val StdoutOutput = if (isWindows) "TEST\r\n" else "TEST ☘\n"

  final def script(duration: FiniteDuration, resultVariable: Option[String] = None): String =
    if (isWindows)
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
    if (isWindows)
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
     js7-common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh \
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
     js7-common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh \
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
    config: Config = ConfigFactory.empty)
  : Resource[Task, ClusterWatchService] =
    Akkas
      .actorSystemResource(clusterWatchId.string)
      .flatMap(implicit actorSystem =>
        ClusterWatchService.resource(
          clusterWatchId,
          admissions.traverse(AkkaHttpControllerApi.admissionToApiResource(_, httpsConfig)),
          config.withFallback(Js7Configuration.defaultConfig)))
}
