package js7.tests.testenv

import com.google.inject.Module
import com.google.inject.util.Modules.EMPTY_MODULE
import com.typesafe.config.ConfigUtil.quoteString
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Files.{createDirectory, createTempDirectory}
import java.nio.file.Path
import java.util.Locale
import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.crypt.{MessageSigner, SignatureVerifier, SignedString}
import js7.base.generic.SecretString
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.{closeOnError, multipleAutoClosing}
import js7.base.utils.Closer.syntax.RichClosersAny
import js7.base.utils.HasCloser
import js7.base.utils.ScalaUtils.RichThrowable
import js7.base.utils.ScalazStyle._
import js7.base.web.Uri
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.scalautil.FileUtils.deleteDirectoryRecursively
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.system.OperatingSystem.isWindows
import js7.common.utils.Exceptions.repeatUntilNoException
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.common.utils.JavaResource
import js7.controller.RunningController
import js7.controller.configuration.ControllerConfiguration
import js7.controller.data.ControllerCommand.{ReplaceRepo, UpdateRepo}
import js7.core.crypt.pgp.PgpSigner
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.controller.ControllerFileBaseds
import js7.data.filebased.{FileBased, FileBasedSigner, TypedPath, VersionId}
import js7.data.job.ExecutablePath
import js7.tests.testenv.DirectoryProvider._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.AtomicBoolean
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class DirectoryProvider(
  agentRefPaths: Seq[AgentRefPath],
  fileBased: Seq[FileBased] = Nil,
  controllerConfig: Config = ConfigFactory.empty,
  agentHttps: Boolean = false,
  agentHttpsMutual: Boolean = false,
  agentConfig: Config = ConfigFactory.empty,
  agentPorts: Iterable[Int] = Nil,
  provideAgentHttpsCertificate: Boolean = false,
  provideAgentClientCertificate: Boolean = false,
  controllerHttpsMutual: Boolean = false,
  controllerClientCertificate: Option[JavaResource] = None,
  signer: MessageSigner = defaultSigner,
  testName: Option[String] = None,
  useDirectory: Option[Path] = None,
  suppressRepo: Boolean = false)
extends HasCloser
{
  val directory = useDirectory.getOrElse(
    createTempDirectory(testName.fold("test-")(_ + "-"))
      .withCloser { dir =>
        repeatUntilNoException(10.s, 10.ms) {  // Windows
          deleteDirectoryRecursively(dir)
        }
      })

  val controller = new ControllerTree(directory / "controller",
    mutualHttps = controllerHttpsMutual, clientTrustStore = controllerClientCertificate)
  val agentToTree: Map[AgentRefPath, AgentTree] =
    agentRefPaths
      .zip(agentPorts ++ Vector.fill(agentRefPaths.size - agentPorts.size)(findFreeTcpPort()))
      .map { case (agentRefPath, port) => agentRefPath ->
        new AgentTree(directory, agentRefPath,
          testName.fold("")(_ + "-") ++ agentRefPath.name,
          port = port,
          https = agentHttps,
          mutualHttps = agentHttpsMutual,
          provideHttpsCertificate = provideAgentHttpsCertificate,
          provideClientCertificate = provideAgentClientCertificate,
          agentConfig)
      }
      .toMap
  val agents: Vector[AgentTree] = agentToTree.values.toVector
  lazy val agentRefs: Vector[AgentRef] = for (a <- agents) yield AgentRef(a.agentRefPath, uri = a.agentConfiguration.localUri)
  private val filebasedHasBeenAdded = AtomicBoolean(false)

  closeOnError(this) {
    controller.createDirectoriesAndFiles()
    writeTrustedSignatureKeys(signer.toVerifier, controller.configDir, "controller.conf")
    agents foreach prepareAgentFiles

    // Agent configurations have already been written by a.createDirectoriesAndFiles()
    controller.configDir / "private" / "private.conf" ++=
      s"""js7.https.keystore {
         |  store-password = "jobscheduler"
         |  key-password = "jobscheduler"
         |}
         |""".stripMargin
  }

  val fileBasedSigner = new FileBasedSigner(signer, ControllerFileBaseds.jsonCodec)

  val sign: FileBased => SignedString = fileBasedSigner.sign

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
    mutualHttps: Boolean = false,
    fileBased: Seq[FileBased] = fileBased,
    name: String = controllerName)
  : Task[RunningController]
  =
    Task.deferFuture(
      RunningController.fromInjector(RunningController.newInjectorForTest(controller.directory, module, config withFallback controllerConfig,
        httpPort = httpPort, httpsPort = httpsPort, mutualHttps = mutualHttps, name = name)))
    .map { runningController =>
      if (!suppressRepo) {
        val myFileBased = agentRefs ++ fileBased
        if (!filebasedHasBeenAdded.getAndSet(true) && myFileBased.nonEmpty) {
          // startController may be called several times. We configure only once.
          runningController.waitUntilReady()
          runningController.executeCommandAsSystemUser(ReplaceRepo(
            Vinitial,
            myFileBased map (_ withVersion Vinitial) map fileBasedSigner.sign)
          ).await(99.s).orThrow
        }
      }
      for (t <- runningController.terminated.failed) {
        scribe.error(t.toStringWithCauses)
        scribe.debug(t.toStringWithCauses, t)
      }
      runningController
    }

  def runAgents[A]()(body: IndexedSeq[RunningAgent] => A): A =
    multipleAutoClosing(agents map (_.agentConfiguration) map RunningAgent.startForTest await 99.s) { agents =>
      val a =
        try body(agents)
        catch { case NonFatal(t) =>
          try agents map (_.terminate()) await 99.s
          catch { case NonFatal(t2) => t.addSuppressed(t2) }
          throw t
        }
      agents map (_.terminate()) await 99.s
      a
    }

  def startAgents(): Future[Seq[RunningAgent]] =
    Future.sequence(agents map (_.agentRefPath) map startAgent)

  def startAgent(agentRefPath: AgentRefPath): Future[RunningAgent] =
    RunningAgent.startForTest(agentToTree(agentRefPath).agentConfiguration)

  def updateRepo(
    controller: RunningController,
    versionId: VersionId,
    change: Seq[FileBased] = Nil,
    delete: Seq[TypedPath] = Nil)
  : Unit =
    controller.executeCommandAsSystemUser(UpdateRepo(
      versionId,
      change map (_ withVersion versionId) map fileBasedSigner.sign,
      delete)
    ).await(99.s).orThrow

  private def controllerName = testName.fold(ControllerConfiguration.DefaultName)(_ + "-Controller")

  def prepareAgentFiles(agentTree: AgentTree): Unit = {
    agentTree.createDirectoriesAndFiles()
    controller.writeAgentAuthentication(agentTree)
    agentTree.writeTrustedSignatureKeys(signer.toVerifier)
  }
}

object DirectoryProvider
{
  coupleScribeWithSlf4j()

  val Vinitial = VersionId("INITIAL")

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

  final class ControllerTree(val directory: Path, mutualHttps: Boolean, clientTrustStore: Option[JavaResource]) extends Tree
  {
    def provideHttpsCertificates(): Unit = {
      configDir / "private/https-keystore.p12" := ControllerKeyStoreResource.contentBytes
      provideTrustStore(AgentTrustStoreResource, "agent-https-truststore.p12")
      for (o <- clientTrustStore) {
        provideTrustStore(o, "client-https-truststore.p12")
        //importKeyStore(configDir / "private/agent-https-truststore.p12", o)
      }
    }

    private def provideTrustStore(resource: JavaResource, filename: String): Unit = {
      val trustStore = configDir / "private" / filename
      trustStore := resource.contentBytes
      configDir / "private/private.conf" ++= s"""
         |js7.https.truststores += {
         |  file = "$trustStore"
         |  store-password = "jobscheduler"
         |}""".stripMargin
    }

    def writeAgentAuthentication(agentTree: AgentTree): Unit =
      (configDir / "private" / "private.conf") ++=
        "js7.auth.agents." + quoteString(agentTree.agentRefPath.string) + " = " + quoteString(agentTree.password.string) + "\n" +
        "js7.auth.agents." + quoteString(agentTree.localUri.toString) + " = " + quoteString(agentTree.password.string) + "\n"
  }

  final class AgentTree(rootDirectory: Path, val agentRefPath: AgentRefPath, name: String,
    port: Int,
    https: Boolean = false, mutualHttps: Boolean = false,
    provideHttpsCertificate: Boolean = false, provideClientCertificate: Boolean = false,
    config: Config = ConfigFactory.empty)
  extends Tree {
    val directory = rootDirectory / agentRefPath.name
    lazy val agentConfiguration = (AgentConfiguration.forTest(directory,
        config,
        httpPort = !https ? port,
        httpsPort = https ? port,
        mutualHttps = mutualHttps))
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
             |js7.https.truststores = [
             |  {
             |    file = "$configDir/private/controller-https-truststore.p12"
             |    store-password = "jobscheduler"
             |  }
             |]""".stripMargin
        }
      }
      configDir / "private" / "private.conf" ++= s"""
         |js7.auth.users {
         |  Controller = ${quoteString("plain:" + password.string)}
         |}
         |js7.https.keystore {
         |  store-password = "jobscheduler"
         |  key-password = "jobscheduler"
         |}
         |""".stripMargin
    }

    def writeTrustedSignatureKeys(verifier: SignatureVerifier): Unit =
      DirectoryProvider.writeTrustedSignatureKeys(verifier, configDir, "agent.conf")

    def writeExecutable(path: ExecutablePath, string: String): Unit =
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

  private def writeTrustedSignatureKeys(verifier: SignatureVerifier, configDir: Path, confFilename: String): Unit = {
    val dir = "private/" + verifier.companion.recommendedKeyDirectoryName
    createDirectory(configDir / dir)
    for ((key, i) <- verifier.keys.zipWithIndex) {
      configDir / dir / (s"key-${i+1}." + verifier.companion.typeName.toLowerCase(Locale.ROOT)) := key
    }
    configDir / confFilename ++=
      s"""js7.configuration.trusted-signature-keys {
         |  ${verifier.companion.typeName} = $${js7.config-directory}"/$dir"
         |}
         |""".stripMargin
  }

  // Following resources have been generated with the command line:
  // common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=controller -config-directory=tests/src/test/resources/js7/tests/controller/config
  val ControllerKeyStoreResource = JavaResource("js7/tests/controller/config/private/https-keystore.p12")
  val ExportedControllerTrustStoreResource = JavaResource("js7/tests/controller/config/export/https-truststore.p12")

  // Following resources have been generated with the command line:
  // common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=agent -config-directory=tests/src/test/resources/js7/tests/agent/config
  private val AgentKeyStoreResource   = JavaResource("js7/tests/agent/config/private/https-keystore.p12")
  private val AgentTrustStoreResource = JavaResource("js7/tests/agent/config/export/https-truststore.p12")

  final def defaultSigner = pgpSigner

  final lazy val pgpSigner: PgpSigner = PgpSigner.forTest
}
