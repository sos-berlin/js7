package js7.tests.testenv

import com.google.inject.Module
import com.google.inject.util.Modules.EMPTY_MODULE
import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.crypt.{MessageSigner, SignatureVerifier, SignedString}
import js7.base.generic.SecretString
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.{closeOnError, multipleAutoClosing}
import js7.base.utils.Closer.syntax.RichClosersAny
import js7.base.utils.HasCloser
import js7.base.utils.ScalaUtils.RichThrowable
import js7.base.utils.ScalazStyle._
import js7.base.web.Uri
import js7.common.log.ScribeUtils
import js7.common.scalautil.FileUtils
import js7.common.scalautil.FileUtils.deleteDirectoryRecursively
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.system.OperatingSystem.isWindows
import js7.common.utils.Exceptions.repeatUntilNoException
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.common.utils.JavaResource
import js7.core.crypt.pgp.PgpSigner
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.filebased.{FileBased, FileBasedSigner, TypedPath, VersionId}
import js7.data.job.ExecutablePath
import js7.data.master.MasterFileBaseds
import js7.master.RunningMaster
import js7.master.configuration.MasterConfiguration
import js7.master.data.MasterCommand.{ReplaceRepo, UpdateRepo}
import js7.tests.testenv.DirectoryProvider._
import com.typesafe.config.ConfigUtil.quoteString
import com.typesafe.config.{Config, ConfigFactory}
import java.lang.ProcessBuilder.Redirect.INHERIT
import java.nio.file.Files.{createDirectory, createTempDirectory}
import java.nio.file.Path
import java.util.Locale
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
  masterConfig: Config = ConfigFactory.empty,
  agentHttps: Boolean = false,
  agentHttpsMutual: Boolean = false,
  agentConfig: Config = ConfigFactory.empty,
  agentPorts: Iterable[Int] = Nil,
  provideAgentHttpsCertificate: Boolean = false,
  provideAgentClientCertificate: Boolean = false,
  masterHttpsMutual: Boolean = false,
  masterClientCertificate: Option[JavaResource] = None,
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

  val master = new MasterTree(directory / "master",
    mutualHttps = masterHttpsMutual, clientCertificate = masterClientCertificate)
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
    master.createDirectoriesAndFiles()
    writeTrustedSignatureKeys(signer.toVerifier, master.configDir, "master.conf")
    agents foreach prepareAgentFiles

    // Agent configurations have already been written by a.createDirectoriesAndFiles()
    (master.configDir / "private" / "private.conf").append(
      s"""js7.https.keystore {
         |  store-password = "js7"
         |  key-password = "js7"
         |}
         |""".stripMargin)
  }

  val fileBasedSigner = new FileBasedSigner(signer, MasterFileBaseds.jsonCodec)

  val sign: FileBased => SignedString = fileBasedSigner.sign

  def run[A](body: (RunningMaster, IndexedSeq[RunningAgent]) => A): A =
    runAgents()(agents =>
      runMaster()(master =>
        body(master, agents)))

  def runMaster[A](httpPort: Option[Int] = Some(findFreeTcpPort()), dontWaitUntilReady: Boolean = false)(body: RunningMaster => A): A = {
    val runningMaster = startMaster(httpPort = httpPort) await 99.s
    try {
      if (!dontWaitUntilReady) {
        runningMaster.waitUntilReady()
      }
      val a = body(runningMaster)
      runningMaster.terminate() await 99.s
      a
    }
    catch { case NonFatal(t) =>
      try runningMaster.terminate() await 99.s
      catch { case NonFatal(tt) if tt ne t => t.addSuppressed(tt) }
      throw t
    }
  }

  def startMaster(
    module: Module = EMPTY_MODULE,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    httpsPort: Option[Int] = None,
    mutualHttps: Boolean = false,
    fileBased: Seq[FileBased] = fileBased,
    name: String = masterName)
  : Task[RunningMaster]
  =
    Task.deferFuture(
      RunningMaster.fromInjector(RunningMaster.newInjectorForTest(master.directory, module, config withFallback masterConfig,
        httpPort = httpPort, httpsPort = httpsPort, mutualHttps = mutualHttps, name = name)))
    .map { runningMaster =>
      if (!suppressRepo) {
        val myFileBased = agentRefs ++ fileBased
        if (!filebasedHasBeenAdded.getAndSet(true) && myFileBased.nonEmpty) {
          // startMaster may be called several times. We configure only once.
          runningMaster.waitUntilReady()
          runningMaster.executeCommandAsSystemUser(ReplaceRepo(
            Vinitial,
            myFileBased map (_ withVersion Vinitial) map fileBasedSigner.sign)
          ).await(99.s).orThrow
        }
      }
      for (t <- runningMaster.terminated.failed) {
        scribe.error(t.toStringWithCauses)
        scribe.debug(t.toStringWithCauses, t)
      }
      runningMaster
    }

  def runAgents[A]()(body: IndexedSeq[RunningAgent] => A): A =
    multipleAutoClosing(agents map (_.agentConfiguration) map RunningAgent.startForTest await 99.s) { agents =>
      val a = body(agents)
      agents map (_.terminate()) await 99.s
      a
    }

  def startAgents(): Future[Seq[RunningAgent]] =
    Future.sequence(agents map (_.agentRefPath) map startAgent)

  def startAgent(agentRefPath: AgentRefPath): Future[RunningAgent] =
    RunningAgent.startForTest(agentToTree(agentRefPath).agentConfiguration)

  def updateRepo(
    master: RunningMaster,
    versionId: VersionId,
    change: Seq[FileBased] = Nil,
    delete: Seq[TypedPath] = Nil)
  : Unit =
    master.executeCommandAsSystemUser(UpdateRepo(
      versionId,
      change map (_ withVersion versionId) map fileBasedSigner.sign,
      delete)
    ).await(99.s).orThrow

  private def masterName = testName.fold(MasterConfiguration.DefaultName)(_ + "-Master")

  def prepareAgentFiles(agentTree: AgentTree): Unit = {
    agentTree.createDirectoriesAndFiles()
    master.writeAgentAuthentication(agentTree)
    agentTree.writeTrustedSignatureKeys(signer.toVerifier)
  }
}

object DirectoryProvider
{
  ScribeUtils.coupleScribeWithSlf4j()

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

  final class MasterTree(val directory: Path, mutualHttps: Boolean, clientCertificate: Option[JavaResource]) extends Tree {
    def provideHttpsCertificate(): Unit = {
      // Keystore
      (configDir / "private/https-keystore.p12") := MasterKeyStoreResource.contentBytes

      // Truststore
      (configDir / "private/private.conf").append("""
        |js7.https.truststore {
        |  store-password = "js7"
        |}""".stripMargin)
      val trustStore = configDir / "private/https-truststore.p12"
      trustStore := AgentTrustStoreResource.contentBytes
      for (o <- clientCertificate) importKeyStore(trustStore, o)
      // KeyStore passwords has been provided
    }

    def writeAgentAuthentication(agentTree: AgentTree): Unit = {
      (configDir / "private" / "private.conf") ++=
        "js7.auth.agents." + quoteString(agentTree.agentRefPath.string) + " = " + quoteString(agentTree.password.string) + "\n" +
        "js7.auth.agents." + quoteString(agentTree.localUri.toString) + " = " + quoteString(agentTree.password.string) + "\n"
    }
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
          (configDir / "private/private.conf").append("""
            |js7.https.truststore {
            |  store-password = "js7"
            |}""".stripMargin)
          (configDir / "private/https-truststore.p12") := MasterTrustStoreResource.contentBytes
        }
      }
      (configDir / "private" / "private.conf").append(s"""
         |js7.auth.users {
         |  Master = ${quoteString("plain:" + password.string)}
         |}
         |js7.https.keystore {
         |  store-password = "js7"
         |  key-password = "js7"
         |}
         |""".stripMargin)
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
  // common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=master -config-directory=tests/src/test/resources/js7/tests/master/config
  val MasterKeyStoreResource = JavaResource("js7/tests/master/config/private/https-keystore.p12")
  val MasterTrustStoreResource = JavaResource("js7/tests/master/config/export/https-truststore.p12")

  // Following resources have been generated with the command line:
  // common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=agent -config-directory=tests/src/test/resources/js7/tests/agent/config
  private val AgentKeyStoreResource   = JavaResource("js7/tests/agent/config/private/https-keystore.p12")
  private val AgentTrustStoreResource = JavaResource("js7/tests/agent/config/export/https-truststore.p12")

  private def importKeyStore(keyStore: Path, add: JavaResource): Unit =
    FileUtils.withTemporaryFile("test-", ".p12") { file =>
      file := add.contentBytes
      importKeyStore(keyStore, file)
    }

  private def importKeyStore(keyStore: Path, add: Path): Unit = {
    val p = new ProcessBuilder("keytool", "-noprompt",
      "-importkeystore",
      "-destkeystore", keyStore.toString,
      "-deststoretype", "pkcs12",
      "-srckeystore", add.toString,
      "-srcstorepass", "js7",
      "-storepass", "js7")
    p.redirectOutput(INHERIT)
    p.redirectError(INHERIT)
    val process = p.start()
    val finished = process.waitFor(99, SECONDS)
    assertThat(finished, "Command 'keytool' takes longer than 99 seconds")
    assertThat(process.exitValue == 0, s"Command 'keytool' returns with exit code ${process.exitValue}")
  }

  final def defaultSigner = pgpSigner

  final lazy val pgpSigner: PgpSigner = PgpSigner.forTest
}
