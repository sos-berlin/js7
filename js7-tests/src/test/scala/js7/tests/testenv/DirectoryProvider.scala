package js7.tests.testenv

import cats.effect.Resource
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.typesafe.config.ConfigUtil.quoteString
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Files.{createDirectories, createDirectory, createTempDirectory}
import java.nio.file.Path
import js7.agent.configuration.AgentConfiguration
import js7.agent.{RunningAgent, TestAgent}
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.configutils.Configs.{HoconStringInterpolator, *}
import js7.base.crypt.{DocumentSigner, SignatureVerifier, Signed, SignedString}
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
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.{closeOnError, multipleAutoClosing}
import js7.base.utils.CatsBlocking.BlockingTaskResource
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.Closer.syntax.RichClosersAny
import js7.base.utils.HasCloser
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.cluster.watch.ClusterWatchService
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.akkautils.Akkas
import js7.common.configuration.Js7Configuration
import js7.common.utils.Exceptions.repeatUntilNoException
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.RunningController
import js7.controller.RunningController.TestWiring
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResources
import js7.controller.configuration.ControllerConfiguration
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.cluster.ClusterWatchId
import js7.data.controller.ControllerState.signableItemJsonCodec
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddVersion, RemoveVersioned}
import js7.data.item.{InventoryItem, ItemOperation, ItemSigner, SignableItem, SignableSimpleItem, UnsignedSimpleItem, VersionId, VersionedItem, VersionedItemPath}
import js7.data.job.RelativePathExecutable
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.proxy.ControllerApi
import js7.service.pgp.PgpSigner
import js7.subagent.BareSubagent
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
  bareSubagents: Map[AgentPath, Seq[SubagentId]] = Map.empty,
  items: Seq[InventoryItem] = Nil,
  controllerConfig: Config = ConfigFactory.empty,
  controllerTestWiring: RunningController.TestWiring = RunningController.TestWiring.empty,
  agentHttps: Boolean = false,
  agentHttpsMutual: Boolean = false,
  agentConfig: Config = ConfigFactory.empty,
  agentPorts: Seq[Int] = Nil,
  backupAgentPorts: Seq[Int] = Nil/*Required for SubagentItem.backupUri*/,
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

  val controller = new ControllerTree(directory / "controller",
    keyStore = controllerKeyStore, trustStores = controllerTrustStores,
    agentHttpsMutual = agentHttpsMutual)
  val agentToTree: Map[AgentPath, AgentTree] = {
    val ports = agentPorts ++ Vector.fill(agentPaths.length - agentPorts.length)(findFreeTcpPort())
    agentPaths
      .zip(ports.zipAll(backupAgentPorts.take(ports.length).map(Some(_)), -999/*unused*/, None))
      .map { case (agentPath, (primaryPort, maybeBackupPort)) =>
        val localSubagentId = toLocalSubagentId(agentPath)
        val localhost = if (agentHttps) "https://localhost" else "http://127.0.0.1"

        val localSubagentItem = SubagentItem(
          localSubagentId, agentPath, disabled = subagentsDisabled,
          uri = Uri(s"$localhost:$primaryPort"),
          backupUri = for (backupPort <- maybeBackupPort) yield Uri(s"$localhost:$backupPort"))

        agentPath ->
          new AgentTree(directory, agentPath,
            localSubagentItem,
            testName.fold("")(_ + "-") ++ localSubagentId.string,
            mutualHttps = agentHttpsMutual,
            provideHttpsCertificate = provideAgentHttpsCertificate,
            provideClientCertificate = provideAgentClientCertificate,
            bareSubagents.getOrElse(agentPath, Nil),
            subagentsDisabled = subagentsDisabled,
            agentConfig)
      }
      .toMap
  }
  val agents: Vector[AgentTree] = agentToTree.values.toVector
  lazy val agentRefs: Vector[AgentRef] =
    for (a <- agents) yield AgentRef(a.agentPath, Seq(a.localSubagentId))
  lazy val subagentItems: Vector[SubagentItem] = agents.flatMap(_.subagentItems)
  lazy val subagentId: SubagentId = agents.head.localSubagentId

  private val itemsHasBeenAdded = AtomicBoolean(false)

  closeOnError(this) {
    controller.createDirectoriesAndFiles()
    writeTrustedSignatureKeys(verifier, controller.configDir, "controller.conf")
    agents foreach prepareAgentFiles
  }

  val itemSigner = new ItemSigner(signer, signableItemJsonCodec)

  def toSignedString[A <: SignableItem](item: A): SignedString =
    itemSigner.toSignedString(item)

  def sign[A <: SignableItem](item: A): Signed[A] =
    itemSigner.sign(item)

  /** Proxy's ControllerApi */
  def controllerApiResource(runningController: RunningController): Resource[Task, ControllerApi] =
    ControllerApi.resource(
      admissionsToApiResources(
        Nel.one(controllerAdmission(runningController)))(
        runningController.actorSystem))

  def controllerAdmission(runningController: RunningController): Admission =
    Admission(runningController.localUri, Some(controller.userAndPassword))

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
          try testController.close()
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
      configAndData = controller.directory,
      config.withFallback(controllerConfig),
      httpPort = httpPort,
      httpsPort = httpsPort,
      name = controllerName)

    def startForTest(runningController: RunningController): Unit = {
      if (!doNotAddItems && (agentRefs.nonEmpty || items.nonEmpty)) {
        if (!itemsHasBeenAdded.getAndSet(true)) {
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
  : A =
    multipleAutoClosing(agents
      .filter(o => agentPaths.contains(o.agentPath))
      .map(_.agentConfiguration)
      .parTraverse(a => TestAgent.start(a))
      .await(99.s))
    { agents =>
      val result =
        try body(agents)
        catch { case NonFatal(t) =>
          // Akka may crash before the caller gets the error so we log the error here
          logger.error(s"💥💥💥 ${t.toStringWithCauses}", t.nullIfNoStackTrace)
          try agents.traverse(_.stop) await 99.s
          catch { case t2: Throwable if t2 ne t => t.addSuppressed(t2) }
          throw t
        }
      agents.traverse(_.terminate()) await 99.s
      result
    }

  def startAgents(testWiring: RunningAgent.TestWiring = RunningAgent.TestWiring.empty)
  : Task[Seq[TestAgent]] =
    agents.parTraverse(a => startAgent(a.agentPath, testWiring))

  def startAgent(
    agentPath: AgentPath,
    testWiring: RunningAgent.TestWiring = RunningAgent.TestWiring.empty)
  : Task[TestAgent] =
    TestAgent.start(
      agentToTree(agentPath).agentConfiguration,
      testWiring)

  def startBareSubagents(): Task[Seq[(BareSubagent, (SubagentId, Task[Unit]))]] =
    agents
      .flatMap(a => a.bareSubagentItems.map(_ -> a.agentConfiguration.config))
      .parTraverse { case (subagentItem, config) =>
        subagentResource(subagentItem, config)
          .allocated
          .map { case (bareSubagent, release) =>
            bareSubagent -> (subagentItem.id -> release.memoize)
          }
      }

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

  def prepareAgentFiles(agentTree: AgentTree): Unit = {
    agentTree.createDirectoriesAndFiles()
    controller.writeAgentAuthentication(agentTree)
    agentTree.writeTrustedSignatureKeys(verifier)
  }

  def subagentResource(
    subagentItem: SubagentItem,
    config: Config = ConfigFactory.empty,
    suffix: String = "",
    suppressSignatureKeys: Boolean = false)
  : Resource[Task, BareSubagent] =
    subagentResource2(subagentItem.id, subagentItem.uri, subagentItem.agentPath,
      config, suffix, suppressSignatureKeys)

  private def subagentResource2(
    subagentId: SubagentId,
    uri: Uri,
    agentPath: AgentPath,
    config: Config = ConfigFactory.empty,
    suffix: String = "",
    suppressSignatureKeys: Boolean = false)
  : Resource[Task, BareSubagent] =
    for {
      dir <- subagentEnvironment(subagentId, suffix = suffix)
      trustedSignatureDir = dir / "config" / "private" /
        verifier.companion.recommendedKeyDirectoryName
      conf = {
        createDirectories(trustedSignatureDir)
        if (!suppressSignatureKeys) provideSignatureKeys(trustedSignatureDir)
        toSubagentConf(
          agentPath,
          dir,
          trustedSignatureDir,
          uri.port.orThrow,
          config,
          name = subagentId.string)
      }
      scheduler <- BareSubagent.threadPoolResource[Task](conf)
      subagent <- BareSubagent.resource(conf.finishAndProvideFiles, scheduler)
    } yield subagent

  private def subagentEnvironment(subagentId: SubagentId, suffix: String): Resource[Task, Path] =
    Resource.make(
      acquire = Task {
        val dir = bareSubagentToDirectory(subagentId, suffix)
        createDirectories(directory / "subagents")
        createDirectory(dir)
        createDirectory(dir / "data")
        createDirectory(dir / "data" / "logs")
        dir
      })(
      release = dir => Task {
        deleteDirectoryRecursively(dir)
      })

  def bareSubagentToDirectory(subagentId: SubagentId, suffix: String = ""): Path =
    directory / "subagents" / (subagentId.string + suffix)

  private def provideSignatureKeys(trustedSignatureDir: Path) =
    for ((key, i) <- verifier.publicKeys.zipWithIndex) {
      val file = trustedSignatureDir / (s"key-${i+1}${verifier.companion.filenameExtension}")
      logger.trace(s"$file := key")
      file := key
    }

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
        .withFallback(SubagentConf.defaultConfig))
}

object DirectoryProvider
{
  private val Vinitial = VersionId("INITIAL")
  private val logger = Logger[this.type]

  def toLocalSubagentId(agentPath: AgentPath): SubagentId =
    SubagentId(agentPath.string + "-0")

  sealed trait Tree {
    val directory: Path
    lazy val configDir = directory / "config"
    lazy val dataDir = directory / "data"
    lazy val stateDir = dataDir / "state"

    def journalFileBase: Path

    private[DirectoryProvider] def createDirectoriesAndFiles(): Unit = {
      createDirectory(directory)
      createDirectory(configDir)
      createDirectory(configDir / "private")
      createDirectory(dataDir)
      createDirectory(dataDir / "work")
    }
  }

  final class ControllerTree(val directory: Path,
    keyStore: Option[JavaResource], trustStores: Iterable[JavaResource], agentHttpsMutual: Boolean)
  extends Tree
  {
    val journalFileBase = stateDir / "controller"
    val userAndPassword = UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD"))
    // TODO Like AgentTree, use this port in startController
    //lazy val port = findFreeTcpPort()
    //lazy val localUri = Uri((if (https) "https://localhost" else "http://127.0.0.1") + ":" + port)

    override private[DirectoryProvider] def createDirectoriesAndFiles(): Unit = {
      super.createDirectoriesAndFiles()
      for (keyStore <- keyStore) {
        configDir / "private/private.conf" ++=
           """js7.web.https.keystore {
             |  store-password = "jobscheduler"
             |  key-password = "jobscheduler"
             |}
             |""".stripMargin
        configDir / "private/https-keystore.p12" := keyStore.contentBytes
        provideTrustStore(AgentTrustStoreResource, "agent-https-truststore.p12")
        for ((o, i) <- trustStores.zipWithIndex) {
          provideTrustStore(o, s"extra-${i+1}-https-truststore.p12")
        }
      }
    }

    private def provideTrustStore(resource: JavaResource, filename: String): Unit = {
      val trustStore = configDir / "private" / filename
      trustStore := resource.contentBytes
      configDir / "private/private.conf" ++= s"""
         |js7.auth.users.${userAndPassword.userId.string}.password = "plain:${userAndPassword.password.string}"
         |js7.web.https.truststores += {
         |  file = ${quoteString(trustStore.toString)}
         |  store-password = "jobscheduler"
         |}
         |""".stripMargin
    }

    def writeAgentAuthentication(agentTree: AgentTree): Unit =
      if (!agentHttpsMutual) {
        val quotedAgentPath = quoteString(agentTree.agentPath.string)
        val quotedPassword = quoteString(agentTree.password.string)
        (configDir / "private" / "private.conf") ++=
          s"js7.auth.agents.$quotedAgentPath = $quotedPassword\n"
      } else {
        // Agent uses the distinguished name of the Controller's HTTPS certificate
      }
  }

  final class AgentTree(rootDirectory: Path,
    val agentPath: AgentPath,
    localSubagentItem: SubagentItem,
    name: String,
    mutualHttps: Boolean = false,
    provideHttpsCertificate: Boolean = false, provideClientCertificate: Boolean = false,
    bareSubagentIds: Seq[SubagentId] = Nil,
    subagentsDisabled: Boolean = false,
    config: Config = ConfigFactory.empty)
  extends Tree {
    val directory = rootDirectory / agentPath.string
    val journalFileBase = stateDir / "agent"

    val localUri = localSubagentItem.uri
    private val port = localSubagentItem.uri.port.orThrow
    private val https = localSubagentItem.uri.string.startsWith("https:")
    lazy val agentConfiguration = AgentConfiguration.forTest(directory,
      name = name,
      config.withFallback(bareSubagentIds
        .map(subagentId => config"""js7.auth.subagents.${subagentId.string} = "AGENT-PASSWORD" """)
        .combineAll),
      httpPort = !https ? port,
      httpsPort = https ? port)
    lazy val password = SecretString(s"$agentPath-PASSWORD")
    lazy val userAndPassword = Some(UserAndPassword(UserId("Controller"), password))
    lazy val executables = configDir / "executables"
    lazy val bareSubagentItems =
      for (subagentId <- bareSubagentIds) yield
        SubagentItem(
          subagentId, agentPath, Uri(s"http://localhost:${findFreeTcpPort()}"),
          disabled = subagentsDisabled)
    lazy val subagentItems = localSubagentItem +: bareSubagentItems

    def localSubagentId: SubagentId =
      localSubagentItem.id

    override def createDirectoriesAndFiles(): Unit = {
      super.createDirectoriesAndFiles()
      createDirectory(executables)
      if (provideHttpsCertificate) {
        (configDir / "private/https-keystore.p12") := AgentKeyStoreResource.contentBytes
        if (provideClientCertificate) {
          configDir / "private/controller-https-truststore.p12" := ExportedControllerTrustStoreResource.contentBytes
          configDir / "private/private.conf" ++= s"""
             |js7.web.https.truststores = [
             |  {
             |    file = $${js7.config-directory}/private/controller-https-truststore.p12
             |    store-password = "jobscheduler"
             |  }
             |]""".stripMargin
        }
      }
      configDir / "private" / "private.conf" ++= s"""
         |js7.auth.users {
         |  Controller {
         |    password = ${quoteString("plain:" + password.string)}
         |    distinguished-names = [
         |      "CN=Primary Controller, DC=primary-controller, DC=DirectoryProvider, DC=tests, DC=js7, DC=sh",
         |      "CN=Backup Controller,DC=backup-controller,DC=HttpsTestBase,DC=tests,DC=js7,DC=sh"
         |    ]
         |  }
         |}
         |js7.web.server.auth.https-client-authentication = $mutualHttps
         |js7.web.https.keystore {
         |  store-password = "jobscheduler"
         |  key-password = "jobscheduler"
         |}
         |""".stripMargin
    }

    def writeTrustedSignatureKeys(verifier: SignatureVerifier): Unit =
      DirectoryProvider.writeTrustedSignatureKeys(verifier, configDir, "agent.conf")

    def writeExecutable(path: RelativePathExecutable, string: String): Unit =
      path.toFile(executables).writeUtf8Executable(string)
  }

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

  private def writeTrustedSignatureKeys(
    verifier: SignatureVerifier,
    configDir: Path,
    confFilename: String)
  : Unit = {
    val dir = "private/" + verifier.companion.recommendedKeyDirectoryName
    createDirectory(configDir / dir)
    for ((key, i) <- verifier.publicKeys.zipWithIndex) {
      val file = configDir / dir / (s"key-${i+1}${verifier.companion.filenameExtension}")
      logger.trace(s"$file := key")
      file := key
    }
    configDir / confFilename ++=
      s"""js7.configuration.trusted-signature-keys {
         |  ${verifier.companion.typeName} = $${js7.config-directory}"/$dir"
         |}
         |""".stripMargin
  }

  /* Following resources have been generated with the command line:
     js7-common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh \
        --distinguished-name="CN=Primary Controller, DC=primary-controller, DC=DirectoryProvider, DC=tests, DC=js7, DC=sh" \
        --alias=controller \
        --host=localhost \
        --config-directory=js7-tests/src/test/resources/js7/tests/controller/config
   */
  val ControllerKeyStoreResource = JavaResource("js7/tests/controller/config/private/https-keystore.p12")
  lazy val controllerClientKeyStoreRef = KeyStoreRef(
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
  private val AgentKeyStoreResource   = JavaResource("js7/tests/agent/config/private/https-keystore.p12")
  private val AgentTrustStoreResource = JavaResource("js7/tests/agent/config/export/https-truststore.p12")

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
