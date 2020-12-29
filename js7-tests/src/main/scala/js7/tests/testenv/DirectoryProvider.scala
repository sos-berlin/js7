package js7.tests.testenv

import com.google.inject.Module
import com.google.inject.util.Modules.EMPTY_MODULE
import com.typesafe.config.ConfigUtil.quoteString
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Files.{createDirectory, createTempDirectory}
import java.nio.file.Path
import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.crypt.{DocumentSigner, SignatureVerifier, Signed, SignedString}
import js7.base.generic.SecretString
import js7.base.problem.Checked._
import js7.base.system.OperatingSystem.isWindows
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.{closeOnError, multipleAutoClosing}
import js7.base.utils.Closer.syntax.RichClosersAny
import js7.base.utils.HasCloser
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.common.akkahttp.https.TrustStoreRef
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.scalautil.FileUtils.deleteDirectoryRecursively
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.Exceptions.repeatUntilNoException
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.common.utils.JavaResource
import js7.controller.RunningController
import js7.controller.configuration.ControllerConfiguration
import js7.controller.data.ControllerCommand.ReplaceRepo
import js7.controller.data.ControllerState.versionedItemJsonCodec
import js7.core.crypt.pgp.PgpSigner
import js7.data.agent.{AgentId, AgentRef}
import js7.data.item.ItemOperation.{AddVersion, VersionedAddOrChange, VersionedDelete}
import js7.data.item.{ItemPath, VersionId, VersionedItem, VersionedItemSigner}
import js7.data.job.RelativeExecutablePath
import js7.tests.testenv.DirectoryProvider._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.AtomicBoolean
import monix.reactive.Observable
import scala.collection.immutable.Iterable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class DirectoryProvider(
  agentIds: Seq[AgentId],
  versionedItems: Seq[VersionedItem] = Nil,
  controllerConfig: Config = ConfigFactory.empty,
  agentHttps: Boolean = false,
  agentHttpsMutual: Boolean = false,
  agentConfig: Config = ConfigFactory.empty,
  agentPorts: Iterable[Int] = Nil,
  provideAgentHttpsCertificate: Boolean = false,
  provideAgentClientCertificate: Boolean = false,
  controllerKeyStore: Option[JavaResource] = Some(ControllerKeyStoreResource),
  controllerTrustStores: Iterable[JavaResource] = Nil,
  signer: DocumentSigner = defaultSigner,
  verifier: SignatureVerifier = defaultVerifier,
  testName: Option[String] = None,
  useDirectory: Option[Path] = None,
  doNotAddItems: Boolean = false)
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
  val agentToTree: Map[AgentId, AgentTree] =
    agentIds
      .zip(agentPorts ++ Vector.fill(agentIds.size - agentPorts.size)(findFreeTcpPort()))
      .map { case (agentId, port) => agentId ->
        new AgentTree(directory, agentId,
          testName.fold("")(_ + "-") ++ agentId.string,
          port = port,
          https = agentHttps,
          mutualHttps = agentHttpsMutual,
          provideHttpsCertificate = provideAgentHttpsCertificate,
          provideClientCertificate = provideAgentClientCertificate,
          agentConfig)
      }
      .toMap
  val agents: Vector[AgentTree] = agentToTree.values.toVector
  lazy val agentRefs: Vector[AgentRef] = for (a <- agents) yield AgentRef(a.agentId, uri = a.agentConfiguration.localUri)
  private val itemHasBeenAdded = AtomicBoolean(false)

  closeOnError(this) {
    controller.createDirectoriesAndFiles()
    writeTrustedSignatureKeys(verifier, controller.configDir, "controller.conf")
    agents foreach prepareAgentFiles
  }

  val itemSigner = new VersionedItemSigner(signer, versionedItemJsonCodec)

  val toSigned: VersionedItem => Signed[VersionedItem] = o => Signed(o, sign(o))
  val sign: VersionedItem => SignedString = itemSigner.sign

  def run[A](body: (RunningController, IndexedSeq[RunningAgent]) => A): A =
    runAgents()(agents =>
      runController()(controller =>
        body(controller, agents)))

  def runController[A](
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    dontWaitUntilReady: Boolean = false,
    config: Config = ConfigFactory.empty)
    (body: RunningController => A)
  : A = {
    val runningController = startController(httpPort = httpPort, config = config) await 99.s
    try {
      if (!dontWaitUntilReady) {
        runningController.waitUntilReady()
      }
      val a = body(runningController)
      runningController.terminate() await 99.s
      a
    }
    catch { case NonFatal(t) =>
      logger.error(t.toStringWithCauses) /* Akka may crash before the caller gets the error so we log the error here */
      try runningController.terminate() await 99.s
      catch { case NonFatal(tt) if tt ne t => t.addSuppressed(tt) }
      throw t
    }
  }

  def startController(
    module: Module = EMPTY_MODULE,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    httpsPort: Option[Int] = None,
    items: Seq[VersionedItem] = versionedItems,
    name: String = controllerName)
  : Task[RunningController]
  =
    Task.deferFuture(
      RunningController.fromInjector(
        RunningController.newInjectorForTest(controller.directory, module, config withFallback controllerConfig,
          httpPort = httpPort, httpsPort = httpsPort, name = name)))
    .map { runningController =>
      if (!doNotAddItems && (agentRefs.nonEmpty || items.nonEmpty)) {
        if (!itemHasBeenAdded.getAndSet(true)) {
          runningController.waitUntilReady()
          runningController.updateSimpleItemsAsSystemUser(agentRefs).await(99.s).orThrow
          if (items.nonEmpty) {
            // startController may be called several times. We configure only once.
            runningController.executeCommandAsSystemUser(ReplaceRepo(
              Vinitial,
              items.map(_ withVersion Vinitial) map itemSigner.sign)
            ).await(99.s).orThrow
          }
        }
      }
      for (t <- runningController.terminated.failed) {
        scribe.error(t.toStringWithCauses)
        scribe.debug(t.toStringWithCauses, t)
      }
      runningController
    }

  def runAgents[A]()(body: IndexedSeq[RunningAgent] => A): A =
    multipleAutoClosing(agents.map(_.agentConfiguration) map RunningAgent.startForTest await 99.s) { agents =>
      val a =
        try body(agents)
        catch { case NonFatal(t) =>
          try agents.map(_.terminate()) await 99.s
          catch { case NonFatal(t2) => t.addSuppressed(t2) }
          throw t
        }
      agents.map(_.terminate()) await 99.s
      a
    }

  def startAgents(): Future[Seq[RunningAgent]] =
    Future.sequence(agents.map(_.agentId) map startAgent)

  def startAgent(agentId: AgentId): Future[RunningAgent] =
    RunningAgent.startForTest(agentToTree(agentId).agentConfiguration)

  def updateVersionedItems(
    controller: RunningController,
    versionId: VersionId,
    change: Seq[VersionedItem] = Nil,
    delete: Seq[ItemPath] = Nil)
  : Unit =
    controller.updateItemsAsSystemUser(
      AddVersion(versionId) +:
        (Observable.fromIterable(change)
          .map(_ withVersion versionId)
          .map(itemSigner.sign)
          .map(VersionedAddOrChange.apply) ++
          Observable.fromIterable(delete)
            .map(VersionedDelete.apply))
    ).await(99.s).orThrow

  private def controllerName = testName.fold(ControllerConfiguration.DefaultName)(_ + "-Controller")

  def prepareAgentFiles(agentTree: AgentTree): Unit = {
    agentTree.createDirectoriesAndFiles()
    controller.writeAgentAuthentication(agentTree)
    agentTree.writeTrustedSignatureKeys(verifier)
  }
}

object DirectoryProvider
{
  val Vinitial = VersionId("INITIAL")
  private val logger = Logger(getClass)

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
    }
  }

  final class ControllerTree(val directory: Path,
    keyStore: Option[JavaResource], trustStores: Iterable[JavaResource], agentHttpsMutual: Boolean)
  extends Tree
  {
    override private[DirectoryProvider] def createDirectoriesAndFiles(): Unit = {
      super.createDirectoriesAndFiles()
      for (keyStore <- keyStore) {
        configDir / "private/private.conf" ++=
          s"""js7.web.https.keystore {
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
         |js7.web.https.truststores += {
         |  file = "$trustStore"
         |  store-password = "jobscheduler"
         |}
         |""".stripMargin
    }

    def writeAgentAuthentication(agentTree: AgentTree): Unit =
      if (!agentHttpsMutual) {
        (configDir / "private" / "private.conf") ++=
          "js7.auth.agents." + quoteString(agentTree.agentId.string) + " = " + quoteString(agentTree.password.string) + "\n" +
          "js7.auth.agents." + quoteString(agentTree.localUri.toString) + " = " + quoteString(agentTree.password.string) + "\n"  /*ClusterWatch*/
      } else {
        // Agent uses the distinguished name of the Controller's HTTPS certificate
      }
  }

  final class AgentTree(rootDirectory: Path, val agentId: AgentId, name: String,
    port: Int,
    https: Boolean = false, mutualHttps: Boolean = false,
    provideHttpsCertificate: Boolean = false, provideClientCertificate: Boolean = false,
    config: Config = ConfigFactory.empty)
  extends Tree {
    val directory = rootDirectory / agentId.string
    lazy val agentConfiguration = (AgentConfiguration.forTest(directory,
        config,
        httpPort = !https ? port,
        httpsPort = https ? port))
      .copy(name = name)
    lazy val localUri = Uri((if (https) "https://localhost" else "http://127.0.0.1") + ":" + port)
    lazy val password = SecretString(Array.fill(8)(Random.nextPrintableChar()).mkString)
    lazy val executables = configDir / "executables"

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
             |    file = "$configDir/private/controller-https-truststore.p12"
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

    def writeExecutable(path: RelativeExecutablePath, string: String): Unit =
      path.toFile(executables).writeExecutable(string)
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
    if (isWindows)  // TODO WINDOWS NOT TESTED
       s"""@echo off
          |:LOOP
          |  if exist "$file" goto FOUND
          |  timeout /t 1 >nul
          |  goto LOOP
          |:FOUND
          |""" + (delete ?? s"del $file\n")
    else
       s"""#!/usr/bin/env bash
          |set -e
          |while [ ! -e '$file' ]; do
          |  sleep 0.1
          |done
          |""".stripMargin + (delete ?? s"rm '$file'\n")

  private def writeTrustedSignatureKeys(verifier: SignatureVerifier, configDir: Path, confFilename: String): Unit = {
    val dir = "private/" + verifier.companion.recommendedKeyDirectoryName
    createDirectory(configDir / dir)
    for ((key, i) <- verifier.publicKeys.zipWithIndex) {
      configDir / dir / (s"key-${i+1}${verifier.companion.filenameExtension}") := key
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
