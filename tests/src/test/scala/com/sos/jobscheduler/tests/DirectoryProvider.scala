package com.sos.jobscheduler.tests

import akka.http.scaladsl.model.Uri
import com.google.inject.Module
import com.google.inject.util.Modules.EMPTY_MODULE
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.base.utils.SyncResource.ops._
import com.sos.jobscheduler.common.scalautil.AutoClosing.{autoClosing, closeOnError, multipleAutoClosing}
import com.sos.jobscheduler.common.scalautil.Closer.ops.RichClosersAny
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.GuavaUtils.stringToInputStreamResource
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.scalautil.{FileUtils, HasCloser}
import com.sos.jobscheduler.common.system.OperatingSystem.{isUnix, isWindows}
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.core.filebased.FileBasedSigner
import com.sos.jobscheduler.core.signature.{PGPCommons, PGPKeyGenerator, PGPUserId}
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.data.filebased.{FileBased, TypedPath, VersionId}
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand.UpdateRepo
import com.sos.jobscheduler.master.data.MasterFileBaseds
import com.sos.jobscheduler.tests.DirectoryProvider._
import com.typesafe.config.ConfigUtil.quoteString
import com.typesafe.config.{Config, ConfigFactory}
import java.io.FileOutputStream
import java.lang.ProcessBuilder.Redirect.INHERIT
import java.nio.file.Files.{createDirectory, createTempDirectory, setPosixFilePermissions}
import java.nio.file.Path
import java.nio.file.attribute.PosixFilePermissions
import java.time.Duration
import java.util.Base64
import java.util.concurrent.TimeUnit.SECONDS
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.AtomicBoolean
import org.bouncycastle.openpgp.PGPSecretKey
import org.scalatest.BeforeAndAfterAll
import scala.collection.immutable.{IndexedSeq, Seq}
import scala.concurrent.Future
import scala.util.Random
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class DirectoryProvider(
  agentPaths: Seq[AgentPath],
  fileBased: Seq[FileBased] = Nil,
  agentHttps: Boolean = false,
  agentHttpsMutual: Boolean = false,
  provideAgentHttpsCertificate: Boolean = false,
  provideAgentClientCertificate: Boolean = false,
  masterHttpsMutual: Boolean = false,
  masterClientCertificate: Option[JavaResource] = None,
  testName: Option[String] = None)
extends HasCloser {

  val directory = createTempDirectory("test-") withCloser deleteDirectoryRecursively
  val master = new MasterTree(directory / "master",
    mutualHttps = masterHttpsMutual, clientCertificate = masterClientCertificate)
  val agentToTree: Map[AgentPath, AgentTree] =
    agentPaths.map { o ⇒ o →
      new AgentTree(directory, o,
        testName.fold("")(_ + "-") ++ o.name,
        https = agentHttps,
        mutualHttps = agentHttpsMutual,
        provideHttpsCertificate = provideAgentHttpsCertificate,
        provideClientCertificate = provideAgentClientCertificate)
    }.toMap
  val agents: Vector[AgentTree] = agentToTree.values.toVector
  private val filebasedHasBeenAdded = AtomicBoolean(false)

  closeOnError(this) {
    master.createDirectoriesAndFiles()
    for (a ← agents) {
      a.createDirectoriesAndFiles()
      (a.config / "private" / "private.conf").append(s"""
         |jobscheduler.auth.users {
         |  Master = ${quoteString("plain:" + a.password.string)}
         |}
         |jobscheduler.https.keystore {
         |  store-password = "jobscheduler"
         |  key-password = "jobscheduler"
         |}
         |""".stripMargin)
    }
    (master.config / "private" / "private.conf").append(
      agentPaths.map(a ⇒
        "jobscheduler.auth.agents." + quoteString(a.string) + " = " + quoteString(agentToTree(a).password.string) + "\n"
      ).mkString +
      s"""jobscheduler.https.keystore {
         |  store-password = "jobscheduler"
         |  key-password = "jobscheduler"
         |}
         |""".stripMargin)
  }

  val fileBasedSigner: FileBasedSigner = {
    val secretKeyPassword = SecretString(Random.nextString(10))
    val secretKey: PGPSecretKey = PGPKeyGenerator.generateSecretKey(PGPUserId("TEST"), secretKeyPassword, keySize = 1024/*fast for test*/)
    val signer = new FileBasedSigner(MasterFileBaseds.jsonCodec, secretKey.getEncoded.asResource, secretKeyPassword)
    autoClosing(new FileOutputStream(master.config / "private" / "trusted-pgp-keys.asc")) { out ⇒
      PGPCommons.writePublicKeyAscii(secretKey.getPublicKey, out)
    }
    signer
  }

  def signStringToBase64(string: String): String =
    Base64.getMimeEncoder.encodeToString(
      fileBasedSigner.pgpSigner.sign(
        stringToInputStreamResource(string)))

  def run(body: (RunningMaster, IndexedSeq[RunningAgent]) ⇒ Unit): Unit =
    runAgents()(agents ⇒
      runMaster()(master ⇒
        body(master, agents)))

  def runMaster()(body: RunningMaster ⇒ Unit): Unit = {
    val runningMaster = startMaster(name = masterName, fileBased = fileBased) await 99.s
    try {
      body(runningMaster)
      runningMaster.terminate() await 99.s
    }
    catch { case NonFatal(t) ⇒
      try runningMaster.terminate() await 99.s
      catch { case NonFatal(tt) if tt ne t ⇒ t.addSuppressed(tt) }
      throw t
    }
  }

  private def startMaster(
    module: Module = EMPTY_MODULE,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(findRandomFreeTcpPort()),
    httpsPort: Option[Int] = None,
    mutualHttps: Boolean = false,
    fileBased: Seq[FileBased] = Nil,
    name: String = masterName)
  : Task[RunningMaster] =
    Task.deferFuture(
      RunningMaster(RunningMaster.newInjectorForTest(master.directory, module, config,
        httpPort = httpPort, httpsPort = httpsPort, mutualHttps = mutualHttps, name = name)))
    .map { runningMaster ⇒
      if (!filebasedHasBeenAdded.getAndSet(true)) {
        val myFileBased = for (a ← agents) yield Agent(a.agentPath % VersionId.Anonymous, uri = a.conf.localUri.toString)
        updateRepo(runningMaster, Some(VersionId("INITIAL")), myFileBased ++ fileBased)
      }
      runningMaster
    }

  def runAgents()(body: IndexedSeq[RunningAgent] ⇒ Unit): Unit =
    multipleAutoClosing(agents map (_.conf) map RunningAgent.startForTest await 10.s) { agents ⇒
      body(agents)
      agents map (_.terminate()) await 99.s
    }

  def startAgents(config: Config = ConfigFactory.empty): Future[Seq[RunningAgent]] =
    Future.sequence(agents map (_.agentPath) map (startAgent(_, config)))

  def startAgent(agentPath: AgentPath, config: Config = ConfigFactory.empty): Future[RunningAgent] =
    RunningAgent.startForTest(agentToTree(agentPath).conf)

  def updateRepo(
    master: RunningMaster,
    versionId: Option[VersionId] = None,
    change: Seq[FileBased] = Nil,
    delete: Set[TypedPath] = Set.empty)
  : Unit =
    master.executeCommandAsSystemUser(
      UpdateRepo(
        versionId,
        change = change map fileBasedSigner.sign,
        delete = delete)
    ).await(99.s).orThrow

  private def masterName = testName.fold(MasterConfiguration.DefaultName)(_ + "-Master")
}

object DirectoryProvider
{
  trait ForScalaTest extends BeforeAndAfterAll with HasCloser {
    this: org.scalatest.Suite ⇒

    protected def agentPaths: Seq[AgentPath]
    protected def agentHttps = false

    protected final lazy val directoryProvider = new DirectoryProvider(agentPaths,
      agentHttps = agentHttps, agentHttpsMutual = agentHttpsMutual,
      provideAgentHttpsCertificate = provideAgentHttpsCertificate, provideAgentClientCertificate = provideAgentClientCertificate,
      masterHttpsMutual = masterHttpsMutual, masterClientCertificate = masterClientCertificate,
      testName = Some(getClass.getSimpleName))

    protected def agentConfig: Config = ConfigFactory.empty
    protected final lazy val agents: Seq[RunningAgent] = directoryProvider.startAgents(agentConfig) await 99.s
    protected final lazy val agent: RunningAgent = agents.head

    protected val masterModule: Module = EMPTY_MODULE
    protected lazy val masterHttpPort: Option[Int] = Some(findRandomFreeTcpPort())
    protected lazy val masterHttpsPort: Option[Int] = None
    protected def agentHttpsMutual = false
    protected def masterHttpsMutual = false
    protected def provideAgentHttpsCertificate = false
    protected def provideAgentClientCertificate = false
    protected def masterClientCertificate: Option[JavaResource] = None
    protected def masterConfig: Config = ConfigFactory.empty
    protected def fileBased: Seq[FileBased] = Nil

    protected final lazy val master: RunningMaster = directoryProvider.startMaster(
      masterModule,
      masterConfig,
      httpPort = masterHttpPort,
      httpsPort = masterHttpsPort,
      mutualHttps = masterHttpsMutual,
      fileBased = fileBased
    ) await 99.s

    protected final lazy val fileBasedSigner = directoryProvider.fileBasedSigner
    protected final val signStringToBase64 = directoryProvider.signStringToBase64 _

    override def beforeAll() = {
      super.beforeAll()
      agents
      master
    }

    override def afterAll() = {
      master.terminate() await 15.s
      master.close()
      agents.map(_.terminate()) await 15.s
      closer.close()
      for (a ← agents) a.close()
      super.afterAll()
      directoryProvider.close()
    }
  }

  sealed trait Tree {
    val directory: Path
    lazy val config = directory / "config"
    lazy val orderGenerators = {
      val dir = config / "order-generators"
      createDirectory(dir)
      dir
    }
    lazy val data = directory / "data"

    private[DirectoryProvider] def createDirectoriesAndFiles(): Unit = {
      createDirectory(directory)
      createDirectory(config)
      createDirectory(config / "private")
      createDirectory(data)
    }
  }

  final class MasterTree(val directory: Path, mutualHttps: Boolean, clientCertificate: Option[JavaResource]) extends Tree {
    def provideHttpsCertificate(): Unit = {
      // Keystore
      (config / "private/https-keystore.p12").contentBytes = MasterKeyStoreResource.contentBytes

      // Truststore
      (config / "private/private.conf").append("""
        |jobscheduler.https.truststore {
        |  store-password = "jobscheduler"
        |}""".stripMargin)
      val trustStore = config / "private/https-truststore.p12"
      trustStore.contentBytes = AgentTrustStoreResource.contentBytes
      for (o ← clientCertificate) importKeyStore(trustStore, o)
      // KeyStore passwords has been provided
    }
  }

  final class AgentTree(rootDirectory: Path, val agentPath: AgentPath, name: String, https: Boolean, mutualHttps: Boolean,
    provideHttpsCertificate: Boolean, provideClientCertificate: Boolean)
  extends Tree {
    val directory = rootDirectory / agentPath.name
    lazy val conf = AgentConfiguration.forTest(directory,
        httpPort = !https ? findRandomFreeTcpPort(),
        httpsPort = https ? findRandomFreeTcpPort(),
        mutualHttps = mutualHttps)
      .copy(name = name)
    lazy val localUri = Uri((if (https) "https://localhost" else "http://127.0.0.1") + ":" + conf.http.head.address.getPort)
    lazy val password = SecretString(Array.fill(8)(Random.nextPrintableChar()).mkString)
    lazy val executables = config / "executables"

    override private[DirectoryProvider] def createDirectoriesAndFiles(): Unit = {
      super.createDirectoriesAndFiles()
      createDirectory(executables)
      if (provideHttpsCertificate) {
        (config / "private/https-keystore.p12").contentBytes = AgentKeyStoreResource.contentBytes
        if (provideClientCertificate) {
          (config / "private/private.conf").append("""
            |jobscheduler.https.truststore {
            |  store-password = "jobscheduler"
            |}""".stripMargin)
          (config / "private/https-truststore.p12").contentBytes = MasterTrustStoreResource.contentBytes
        }
        // KeyStore passwords has been provided by DirectoryProvider (must happen early)
      }
    }

    def writeExecutable(path: ExecutablePath, string: String): Unit = {
      val file = path.toFile(executables)
      file := string
      if (isUnix) setPosixFilePermissions(file, PosixFilePermissions.fromString("rwx------"))
    }
  }

  final val StdoutOutput = if (isWindows) "TEST\r\n" else "TEST ☘\n"

  final def script(duration: Duration, resultVariable: Option[String] = None) =
    if (isWindows)
      (s"""@echo off
          |echo ${StdoutOutput.trim}
          |ping -n ${1 + (duration + 999999.µs).toMillis / 1000} 127.0.0.1 >nul""" +
          resultVariable.fold("")(o ⇒ s"""|echo result=SCRIPT-VARIABLE-%SCHEDULER_PARAM_${o.toUpperCase}% >>"%SCHEDULER_RETURN_VALUES%"""")
      ).stripMargin
    else
      (s"""echo ${StdoutOutput.trim}
          |sleep ${duration.toSecondsString}""" +
          resultVariable.fold("")(o ⇒ s"""|echo "result=SCRIPT-VARIABLE-$$SCHEDULER_PARAM_${o.toUpperCase}" >>"$$SCHEDULER_RETURN_VALUES"""")
      ).stripMargin

  // Following resources have been generated with the command line:
  // common/src/main/resources/com/sos/jobscheduler/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=master -config-directory=tests/src/test/resources/com/sos/jobscheduler/tests/master/config
  val MasterKeyStoreResource = JavaResource("com/sos/jobscheduler/tests/master/config/private/https-keystore.p12")
  val MasterTrustStoreResource       = JavaResource("com/sos/jobscheduler/tests/master/config/export/https-truststore.p12")

  // Following resources have been generated with the command line:
  // common/src/main/resources/com/sos/jobscheduler/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=agent -config-directory=tests/src/test/resources/com/sos/jobscheduler/tests/agent/config
  private val AgentKeyStoreResource   = JavaResource("com/sos/jobscheduler/tests/agent/config/private/https-keystore.p12")
  private val AgentTrustStoreResource = JavaResource("com/sos/jobscheduler/tests/agent/config/export/https-truststore.p12")

  private def importKeyStore(keyStore: Path, add: JavaResource): Unit =
    FileUtils.withTemporaryFile("test-", ".p12") { file ⇒
      file.contentBytes = add.contentBytes
      importKeyStore(keyStore, file)
    }

  private def importKeyStore(keyStore: Path, add: Path): Unit = {
    val p = new ProcessBuilder("keytool", "-noprompt",
      "-importkeystore",
      "-destkeystore", keyStore.toString,
      "-deststoretype", "pkcs12",
      "-srckeystore", add.toString,
      "-srcstorepass", "jobscheduler",
      "-storepass", "jobscheduler")
    p.redirectOutput(INHERIT)
    p.redirectError(INHERIT)
    val process = p.start()
    val finished = process.waitFor(9, SECONDS)
    assert(finished)
    assert(process.exitValue == 0)
  }
}
