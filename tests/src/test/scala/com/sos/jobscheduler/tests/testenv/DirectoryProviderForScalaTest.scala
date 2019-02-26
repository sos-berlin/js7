package com.sos.jobscheduler.tests.testenv

import com.google.inject.Module
import com.google.inject.util.Modules.EMPTY_MODULE
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.core.crypt.MessageSigner
import com.sos.jobscheduler.core.message.ProblemCodeMessages
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.{FileBased, TypedPath, VersionId}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.MasterCommand.UpdateRepo
import com.typesafe.config.{Config, ConfigFactory}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll
import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
trait DirectoryProviderForScalaTest extends BeforeAndAfterAll with HasCloser {
  this: org.scalatest.Suite ⇒

  ProblemCodeMessages.initialize()

  protected def agentPaths: Seq[AgentPath]
  protected def agentHttps = false

  protected final lazy val directoryProvider = new DirectoryProvider(agentPaths,
    agentHttps = agentHttps, agentHttpsMutual = agentHttpsMutual,
    provideAgentHttpsCertificate = provideAgentHttpsCertificate, provideAgentClientCertificate = provideAgentClientCertificate,
    masterHttpsMutual = masterHttpsMutual, masterClientCertificate = masterClientCertificate,
    signer = signer,
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
  protected def fileBased: Seq[FileBased]
  protected def signer: MessageSigner = DirectoryProvider.defaultSigner

  protected final lazy val master: RunningMaster = directoryProvider.startMaster(
    masterModule,
    masterConfig,
    httpPort = masterHttpPort,
    httpsPort = masterHttpsPort,
    mutualHttps = masterHttpsMutual,
    fileBased = fileBased
  ) await 99.s

  protected final def sign(fileBased: FileBased) = directoryProvider.sign(fileBased)

  override def beforeAll() = {
    super.beforeAll()
    directoryProvider
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

  private val usedVersionIds = mutable.Set[VersionId]()

  final def updateRepo(change: Seq[FileBased]): VersionId =
    updateRepo(change, Nil)

  final def updateRepo(
    change: Seq[FileBased],
    delete: Seq[TypedPath])
  : VersionId = {
    val versionId = VersionId.generate(usedVersionIds)
    updateRepo(versionId, change, delete)
    versionId
  }

  final def updateRepo(
    versionId: VersionId,
    change: Seq[FileBased] = Nil,
    delete: Seq[TypedPath] = Nil)
  : Unit = {
    usedVersionIds += versionId
    master.executeCommandAsSystemUser(UpdateRepo(
      versionId,
      change map (_ withVersion versionId) map directoryProvider.fileBasedSigner.sign,
      delete)
    ).await(99.s).orThrow
  }
}
