package com.sos.jobscheduler.tests.testenv

import akka.http.scaladsl.model.Uri
import com.google.inject.Module
import com.google.inject.util.Modules.EMPTY_MODULE
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.scalautil.AutoClosing.{closeOnError, multipleAutoClosing}
import com.sos.jobscheduler.common.scalautil.Closer.ops.RichClosersAny
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.scalautil.{FileUtils, HasCloser}
import com.sos.jobscheduler.common.system.OperatingSystem.{isUnix, isWindows}
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.core.crypt.pgp.PgpSigner
import com.sos.jobscheduler.core.crypt.{MessageSigner, SignatureVerifier}
import com.sos.jobscheduler.core.filebased.FileBasedSigner
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.data.crypt.SignedString
import com.sos.jobscheduler.data.filebased.{FileBased, TypedPath, VersionId}
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.master.MasterFileBaseds
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand.{ReplaceRepo, UpdateRepo}
import com.sos.jobscheduler.tests.testenv.DirectoryProvider._
import com.typesafe.config.ConfigUtil.quoteString
import com.typesafe.config.{Config, ConfigFactory}
import java.lang.ProcessBuilder.Redirect.INHERIT
import java.nio.file.Files.{createDirectory, createTempDirectory, setPosixFilePermissions}
import java.nio.file.Path
import java.nio.file.attribute.PosixFilePermissions
import java.time.Duration
import java.util.concurrent.TimeUnit.SECONDS
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.AtomicBoolean
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
  signer: MessageSigner = defaultSigner,
  testName: Option[String] = None,
  useDirectory: Option[Path] = None)
extends HasCloser {

  val directory = useDirectory getOrElse (createTempDirectory("test-") withCloser deleteDirectoryRecursively)
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
  lazy val agentFileBased: Vector[Agent] = for (a ← agents) yield Agent(a.agentPath, uri = a.conf.localUri.toString)
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
    useSignatureVerifier(signer.toVerifier)
  }

  val fileBasedSigner = new FileBasedSigner(signer, MasterFileBaseds.jsonCodec)

  val sign: FileBased => SignedString = fileBasedSigner.sign

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

  private[testenv] def startMaster(
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
      val myFileBased = agentFileBased ++ fileBased
      if (!filebasedHasBeenAdded.getAndSet(true) && myFileBased.nonEmpty) {
        // startMaster may be called several times. We configure only once.
        runningMaster.executeCommandAsSystemUser(ReplaceRepo(
          Vinitial,
          (agentFileBased ++ fileBased) map (_ withVersion Vinitial) map fileBasedSigner.sign)
        ).await(99.s).orThrow
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

  private def useSignatureVerifier(verifier: SignatureVerifier): Unit = {
    val keyFile = "private/" + verifier.companion.recommendedKeyFileName
    for (config <- master.config +: agents.map(_.config)) {
      config / keyFile := verifier.key
    }
    for (conf <- master.config / "master.conf" +: agents.map(_.config / "agent.conf")) {
      conf ++=
        s"""|jobscheduler.configuration.trusted-signature-keys {
           |  ${verifier.companion.typeName} = $${jobscheduler.config-directory}"/$keyFile"
           |}
           |""".stripMargin
    }
  }
}

object DirectoryProvider
{
  val Vinitial = VersionId("INITIAL")

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

  final def defaultSigner = pgpSigner

  final lazy val pgpSigner: PgpSigner = PgpSigner.forTest
}
