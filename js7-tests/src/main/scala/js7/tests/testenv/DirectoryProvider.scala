package js7.tests.testenv

import cats.effect.Resource
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import com.google.inject.Module
import com.google.inject.util.Modules.EMPTY_MODULE
import com.typesafe.config.ConfigUtil.quoteString
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Files.{createDirectories, createDirectory, createTempDirectory}
import java.nio.file.Path
import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.configutils.Configs.{HoconStringInterpolator, *}
import js7.base.crypt.{DocumentSigner, SignatureVerifier, Signed, SignedString}
import js7.base.generic.SecretString
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.https.TrustStoreRef
import js7.base.log.Logger
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.problem.Checked.*
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.{closeOnError, multipleAutoClosing}
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.Closer.syntax.RichClosersAny
import js7.base.utils.HasCloser
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.utils.Exceptions.repeatUntilNoException
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.RunningController
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResources
import js7.controller.configuration.ControllerConfiguration
import js7.data.agent.{AgentPath, AgentRef}
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
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.util.Random
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
  controllerModule: Module = EMPTY_MODULE,
  agentHttps: Boolean = false,
  agentHttpsMutual: Boolean = false,
  agentConfig: Config = ConfigFactory.empty,
  agentPorts: Iterable[Int] = Nil,
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
  val agentToTree: Map[AgentPath, AgentTree] =
    agentPaths
      .zip(agentPorts ++ Vector.fill(agentPaths.size - agentPorts.size)(findFreeTcpPort()))
      .map { case (agentPath, port) =>
        val localSubagentId = toLocalSubagentId(agentPath)
        agentPath ->
          new AgentTree(directory, agentPath,
            localSubagentId,
            testName.fold("")(_ + "-") ++ localSubagentId.string,
            port = port,
            https = agentHttps,
            mutualHttps = agentHttpsMutual,
            provideHttpsCertificate = provideAgentHttpsCertificate,
            provideClientCertificate = provideAgentClientCertificate,
            bareSubagents.getOrElse(agentPath, Nil),
            subagentsDisabled = subagentsDisabled,
            agentConfig)
      }
      .toMap
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
  def newControllerApi(runningController: RunningController): ControllerApi =
    controllerApiResource(runningController).allocated.await(99.s)._1  // Caller must stop it

  /** Proxy's ControllerApi */
  def controllerApiResource(runningController: RunningController): Resource[Task, ControllerApi] =
    ControllerApi.resource(
      admissionsToApiResources(
        Nel.one(controllerAdmission(runningController)))(
        runningController.actorSystem))

  def controllerAdmission(runningController: RunningController): Admission =
    Admission(runningController.localUri, Some(controller.userAndPassword))

  def run[A](body: (RunningController, IndexedSeq[RunningAgent]) => A): A =
    runAgents(scheduler = scheduler)(agents =>
      runController()(controller =>
        body(controller, agents)))

  def runController[A](
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    dontWaitUntilReady: Boolean = false,
    config: Config = ConfigFactory.empty)
    (body: RunningController => A)
  : A = {
    val runningController = startController(httpPort = httpPort, config = config) await 99.s
    val result =
      try {
        if (!dontWaitUntilReady) {
          runningController.waitUntilReady()
        }
        body(runningController)
      } catch { case NonFatal(t) =>
        logger.error(t.toStringWithCauses) /* Akka may crash before the caller gets the error so we log the error here */
        try runningController.terminate() await 99.s
        catch { case t2: Throwable if t2 ne t => t.addSuppressed(t2) }
        throw t
      }
    try runningController.terminate() await 99.s
    catch { case NonFatal(t) =>
      logger.error(t.toStringWithCauses) /* Akka may crash before the caller gets the error so we log the error here */
      try runningController.close()
      catch { case t2: Throwable if t2 ne t => t.addSuppressed(t2) }
      throw t
    }
    result
  }

  def startController(
    module: Module = controllerModule,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    httpsPort: Option[Int] = None,
    items: Seq[InventoryItem] = items,
    name: String = controllerName,
    scheduler: Option[Scheduler] = scheduler)
  : Task[RunningController]
  =
    Task.deferFuture(
      RunningController.fromInjector(
        RunningController.newInjectorForTest(controller.directory, module, config withFallback controllerConfig,
          httpPort = httpPort, httpsPort = httpsPort, name = name, scheduler)))
    .map { runningController =>
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
        scribe.error(t.toStringWithCauses)
        scribe.debug(t.toStringWithCauses, t)
      }
      runningController
    }

  def runAgents[A](
    agentPaths: Seq[AgentPath] = DirectoryProvider.this.agentPaths,
    scheduler: Option[Scheduler] = scheduler)(
    body: IndexedSeq[RunningAgent] => A)
  : A =
    multipleAutoClosing(agents
      .filter(o => agentPaths.contains(o.agentPath))
      .map(_.agentConfiguration)
      .traverse(RunningAgent.startForTest(_, scheduler))
      .await(99.s))
    { agents =>
      val result =
        try body(agents)
        catch { case NonFatal(t) =>
          logger.error(t.toStringWithCauses) /* Akka may crash before the caller gets the error so we log the error here */
          try agents.traverse(_.terminate()) await 99.s
          catch { case t2: Throwable if t2 ne t => t.addSuppressed(t2) }
          throw t
        }
      agents.traverse(_.terminate()) await 99.s
      result
    }

  def startAgents(module: Module = EMPTY_MODULE): Future[Seq[RunningAgent]] =
    Future.sequence(agents.map(_.agentPath).map(startAgent(_, module)))

  def startAgent(agentPath: AgentPath, module: Module = EMPTY_MODULE): Future[RunningAgent] =
    RunningAgent.startForTest(
      agentToTree(agentPath).agentConfiguration,
      module = module,
      scheduler = scheduler)

  def startBareSubagents(): Task[Seq[(BareSubagent, (SubagentId, Task[Unit]))]] =
    agents
      .flatMap(a => a.bareSubagentItems.map(_ -> a.agentConfiguration.config))
      .traverse { case (subagentItem, config) =>
        subagentResource(subagentItem, config)
          .allocated
          .map { case (bareSubagent, release) =>
            bareSubagent -> (subagentItem.id -> release.memoize)
          }
      }

  def updateVersionedItems(
    controller: RunningController,
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
  val Vinitial = VersionId("INITIAL")
  private val logger = Logger(getClass)

  def toLocalSubagentId(agentPath: AgentPath): SubagentId =
    SubagentId(agentPath.string + "-0")

  sealed trait Tree {
    val directory: Path
    lazy val configDir = directory / "config"
    lazy val dataDir = directory / "data"
    lazy val stateDir = dataDir / "state"

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
    val userAndPassword = UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD"))

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
        (configDir / "private" / "private.conf") ++=
          "js7.auth.agents." + quoteString(agentTree.agentPath.string) + " = " + quoteString(agentTree.password.string) + "\n" +
          "js7.auth.agents." + quoteString(agentTree.localUri.toString) + " = " + quoteString(agentTree.password.string) + "\n"  /*ClusterWatch*/
      } else {
        // Agent uses the distinguished name of the Controller's HTTPS certificate
      }
  }

  final class AgentTree(rootDirectory: Path,
    val agentPath: AgentPath,
    val localSubagentId: SubagentId,
    name: String,
    port: Int,
    https: Boolean = false, mutualHttps: Boolean = false,
    provideHttpsCertificate: Boolean = false, provideClientCertificate: Boolean = false,
    bareSubagentIds: Seq[SubagentId] = Nil,
    subagentsDisabled: Boolean = false,
    config: Config = ConfigFactory.empty)
  extends Tree {
    val directory = rootDirectory / agentPath.string
    lazy val agentConfiguration = AgentConfiguration.forTest(directory,
      name = name,
      config.withFallback(bareSubagentIds
        .map(subagentId => config"""js7.auth.subagents.${subagentId.string} = "AGENT-PASSWORD" """)
        .combineAll),
      httpPort = !https ? port,
      httpsPort = https ? port)
    lazy val localUri = Uri((if (https) "https://localhost" else "http://127.0.0.1") + ":" + port)
    lazy val password = SecretString(Array.fill(8)(Random.nextPrintableChar()).mkString)
    lazy val userAndPassword = Some(UserAndPassword(UserId("Controller"), password))
    lazy val executables = configDir / "executables"

    private lazy val localSubagentItem = SubagentItem(
      localSubagentId, agentPath, uri = agentConfiguration.localUri, disabled = subagentsDisabled)
    lazy val bareSubagentItems =
      for (subagentId <- bareSubagentIds) yield
        SubagentItem(
          subagentId, agentPath, Uri(s"http://localhost:${findFreeTcpPort()}"),
          disabled = subagentsDisabled)
    lazy val subagentItems = localSubagentItem +: bareSubagentItems

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
  val ExportedControllerTrustStoreResource = JavaResource("js7/tests/controller/config/export/https-truststore.p12")
  lazy val ExportedControllerTrustStoreRef = TrustStoreRef(ExportedControllerTrustStoreResource.url, SecretString("jobscheduler"))

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
}
