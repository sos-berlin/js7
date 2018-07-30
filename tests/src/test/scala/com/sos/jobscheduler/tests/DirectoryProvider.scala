package com.sos.jobscheduler.tests

import akka.http.scaladsl.model.Uri
import com.google.inject.Module
import com.google.inject.util.Modules.EMPTY_MODULE
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.scheduler.job.{JobConfiguration, JobScript}
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.scalautil.AutoClosing.{closeOnError, multipleAutoClosing}
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.scalautil.{FileUtils, HasCloser}
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.data.filebased.{FileBased, SourceType, TypedPath, VersionId}
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.tests.DirectoryProvider._
import com.typesafe.config.ConfigUtil.quoteString
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.syntax.EncoderOps
import io.circe.{Json, ObjectEncoder}
import java.lang.ProcessBuilder.Redirect.INHERIT
import java.nio.file.Files.createTempDirectory
import java.nio.file.{Files, Path}
import java.time.Duration
import java.util.concurrent.TimeUnit.SECONDS
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll
import scala.collection.immutable.{IndexedSeq, Seq}
import scala.concurrent.Future
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class DirectoryProvider(
  agentPaths: Seq[AgentPath],
  agentHttps: Boolean = false,
  agentHttpsMutual: Boolean = false,
  provideAgentClientCertificate: Boolean = false,
  masterHttpsMutual: Boolean = false,
  masterClientCertificate: Option[JavaResource] = None)
extends HasCloser {

  val directory = createTempDirectory("test-") withCloser deleteDirectoryRecursively
  val master = new MasterTree(directory / "master",
    mutualHttps = masterHttpsMutual, clientCertificate = masterClientCertificate)
  val agentToTree: Map[AgentPath, AgentTree] =
    agentPaths.map { o ⇒ o →
      new AgentTree(directory, o, https = agentHttps,
        mutualHttps = agentHttpsMutual,
        provideClientCertificate = provideAgentClientCertificate)
    }.toMap
  val agents: Vector[AgentTree] = agentToTree.values.toVector

  closeOnError(this) {
    master.createDirectories()
    for (a ← agents) {
      a.createDirectories()
      (a.config / "private" / "private.conf").append(
        s"""jobscheduler.auth.users {
           |  Master = ${quoteString("plain:" + a.password.string)}
           |}
           |jobscheduler.webserver.https.keystore {
           |  store-password = "jobscheduler"
           |  key-password = "jobscheduler"
           |}
         """.stripMargin)
    }
    (master.config / "private" / "private.conf").append(
      agentPaths.map(a ⇒
        "jobscheduler.auth.agents." + quoteString(a.string) + " = " + quoteString(agentToTree(a).password.string) + "\n"
      ).mkString +
      s"""jobscheduler.webserver.https.keystore {
         |  store-password = "jobscheduler"
         |  key-password = "jobscheduler"
         |}
         """.stripMargin)
    for (a ← agents) {
      val file = master.fileBasedDirectory / s"${a.agentPath.withoutStartingSlash}.agent.json"
      Files.createDirectories(file.getParent)
      file.contentString = Agent(AgentPath.NoId, uri = a.conf.localUri.toString).asJson.toPrettyString
    }
  }

  def run(body: (RunningMaster, IndexedSeq[RunningAgent]) ⇒ Unit): Unit =
    runAgents()(agents ⇒
      runMaster()(master ⇒
        body(master, agents)))

  def runMaster(eventCollector: Option[TestEventCollector] = None)(body: RunningMaster ⇒ Unit): Unit =
    RunningMaster.runForTest(master.directory, eventCollector)(body)

  def startMaster(
    module: Module = EMPTY_MODULE,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(findRandomFreeTcpPort()),
    httpsPort: Option[Int] = None,
    mutualHttps: Boolean = false)
  : Task[RunningMaster] =
    Task.deferFuture(
      RunningMaster(RunningMaster.newInjectorForTest(master.directory, module, config,
        httpPort = httpPort, httpsPort = httpsPort, mutualHttps = mutualHttps)))

  def runAgents()(body: IndexedSeq[RunningAgent] ⇒ Unit): Unit =
    multipleAutoClosing(agents map (_.conf) map RunningAgent.startForTest await 10.s) { agents ⇒
      body(agents)
      agents map (_.terminate()) await 99.s
    }

  def startAgents(config: Config = ConfigFactory.empty): Future[Seq[RunningAgent]] =
    Future.sequence(agents map (_.agentPath) map (startAgent(_, config)))

  def startAgent(agentPath: AgentPath, config: Config = ConfigFactory.empty): Future[RunningAgent] =
    RunningAgent.startForTest(agentToTree(agentPath).conf)
}

object DirectoryProvider
{
  trait ForScalaTest extends BeforeAndAfterAll with HasCloser {
    this: org.scalatest.Suite ⇒

    protected def agentPaths: Seq[AgentPath]
    protected def agentHttps = false

    protected final lazy val directoryProvider = new DirectoryProvider(agentPaths,
      agentHttps = agentHttps, agentHttpsMutual = agentHttpsMutual, provideAgentClientCertificate = provideAgentClientCertificate,
      masterHttpsMutual = masterHttpsMutual, masterClientCertificate = masterClientCertificate)

    protected def agentConfig: Config = ConfigFactory.empty
    protected final lazy val agents: Seq[RunningAgent] = directoryProvider.startAgents(agentConfig) await 99.s
    protected final lazy val agent: RunningAgent = agents.head

    protected val masterModule: Module = EMPTY_MODULE
    protected lazy val masterHttpPort: Option[Int] = Some(findRandomFreeTcpPort())
    protected lazy val masterHttpsPort: Option[Int] = None
    protected def agentHttpsMutual = false
    protected def masterHttpsMutual = false
    protected def provideAgentClientCertificate = false
    protected def masterClientCertificate: Option[JavaResource] = None
    protected def masterConfig: Config = ConfigFactory.empty

    protected final lazy val master: RunningMaster = directoryProvider.startMaster(
      masterModule,
      masterConfig,
      httpPort = masterHttpPort,
      httpsPort = masterHttpsPort,
      mutualHttps = masterHttpsMutual
    ) await 99.s

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
    lazy val fileBasedDirectory = config / "live"
    lazy val orderGenerators = {
      val dir = config / "order-generators"
      Files.createDirectory(dir)
      dir
    }
    lazy val data = directory / "data"

    private[DirectoryProvider] def createDirectories(): Unit = {
      Files.createDirectories(fileBasedDirectory)
      Files.createDirectory(config / "private")
      Files.createDirectory(data)
    }

    def writeJson[A <: FileBased { type Self = A }: ObjectEncoder](fileBased: A): Unit = {
      require(!fileBased.id.path.isAnonymous, "writeJson: Missing path")
      require(fileBased.id.versionId.isAnonymous, "writeJson accepts only VersionId.Anonymous")
      file(fileBased.path, SourceType.Json).contentString =
        Json.fromJsonObject(implicitly[ObjectEncoder[A]].encodeObject(fileBased.withoutId)).toPrettyString
    }

    def writeTxt(path: TypedPath, content: String): Unit =
      file(path, SourceType.Txt).contentString = content

    def file(path: TypedPath, t: SourceType): Path =
      fileBasedDirectory resolve path.toFile(t)
  }

  final class MasterTree(val directory: Path, mutualHttps: Boolean, clientCertificate: Option[JavaResource]) extends Tree {
    def provideHttpsCertificate(): Unit = {
      val keyStore = config / "private/https-keystore.p12"
      keyStore.contentBytes = MasterKeyStoreResource.contentBytes
      importKeyStore(keyStore, AgentTrustStoreResource)
      for (o ← clientCertificate) importKeyStore(keyStore, o)
      // KeyStore passwords has been provided
    }
  }

  final class AgentTree(rootDirectory: Path, val agentPath: AgentPath, https: Boolean, mutualHttps: Boolean, provideClientCertificate: Boolean)
  extends Tree {
    val directory = rootDirectory / agentPath.name
    lazy val conf = AgentConfiguration.forTest(directory,
        httpPort = !https ? findRandomFreeTcpPort(),
        httpsPort = https ? findRandomFreeTcpPort(),
        mutualHttps = mutualHttps)
      .copy(name = agentPath.name)
    lazy val localUri = Uri((if (https) "https://localhost" else "http://127.0.0.1") + ":" + conf.http.head.address.getPort)
    lazy val password = SecretString(Array.fill(8)(Random.nextPrintableChar()).mkString)

    def provideHttpsCertificate(): Unit = {
      val keyStore = config / "private/https-keystore.p12"
      keyStore.contentBytes = AgentKeyStoreResource.contentBytes
      if (provideClientCertificate) {
        importKeyStore(keyStore, MasterTrustStoreResource)
      }
      // KeyStore passwords has been provided by DirectoryProvider (must happen early)
    }
  }

  final def jobJson(duration: Duration = 0.s, variables: Map[String, String] = Map.empty, resultVariable: Option[String] = None) =
    jobConfiguration(JobPath.NoId.path, duration, variables).asJson.toPrettyString

  final def jobConfiguration(jobPath: JobPath, duration: Duration = 0.s, variables: Map[String, String] = Map.empty, resultVariable: Option[String] = None) =
    JobConfiguration(
      jobPath % VersionId.Anonymous,
      JobScript(script(duration, resultVariable)),
      variables,
      taskLimit = 10)

  final val StdoutOutput = if (isWindows) "TEST\r\n" else "TEST ☘\n"

  private def script(duration: Duration, resultVariable: Option[String]) =
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
  private val MasterKeyStoreResource = JavaResource("com/sos/jobscheduler/tests/master/config/private/https-keystore.p12")
  val MasterTrustStoreResource = JavaResource("com/sos/jobscheduler/tests/master/config/export/https-truststore.p12")

  // Following resources have been generated with the command line:
  // common/src/main/resources/com/sos/jobscheduler/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=agent -config-directory=tests/src/test/resources/com/sos/jobscheduler/tests/agent/config
  private val AgentKeyStoreResource = JavaResource("com/sos/jobscheduler/tests/agent/config/private/https-keystore.p12")
  private val AgentTrustStoreResource = JavaResource("com/sos/jobscheduler/tests/agent/config/export/https-truststore.p12")

  private[tests] def importKeyStore(keyStore: Path, add: JavaResource): Unit =
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
