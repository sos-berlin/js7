package com.sos.jobscheduler.tests.testenv

import com.google.inject.Module
import com.google.inject.util.Modules.EMPTY_MODULE
import com.sos.jobscheduler.base.utils.HasCloser
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPort
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.core.crypt.MessageSigner
import com.sos.jobscheduler.core.message.ProblemCodeMessages
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.filebased.FileBased
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.BeforeAndAfterAll
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
trait DirectoryProviderForScalaTest extends BeforeAndAfterAll with HasCloser {
  this: org.scalatest.Suite =>

  ProblemCodeMessages.initialize()

  protected def agentRefPaths: Seq[AgentRefPath]
  protected def agentHttps = false
  protected def suppressRepoInitialization = false

  protected final lazy val directoryProvider = new DirectoryProvider(agentRefPaths,
    fileBased = fileBased,
    masterConfig = masterConfig,
    agentHttps = agentHttps, agentHttpsMutual = agentHttpsMutual,
    agentConfig = agentConfig,
    provideAgentHttpsCertificate = provideAgentHttpsCertificate, provideAgentClientCertificate = provideAgentClientCertificate,
    masterHttpsMutual = masterHttpsMutual, masterClientCertificate = masterClientCertificate,
    signer = signer,
    testName = Some(getClass.getSimpleName),
    suppressRepo = suppressRepoInitialization)

  protected def agentConfig: Config = ConfigFactory.empty

  protected val masterModule: Module = EMPTY_MODULE
  protected lazy val masterHttpPort: Option[Int] = Some(findFreeTcpPort())
  protected lazy val masterHttpsPort: Option[Int] = None
  protected def agentHttpsMutual = false
  protected def masterHttpsMutual = false
  protected def provideAgentHttpsCertificate = false
  protected def provideAgentClientCertificate = false
  protected def masterClientCertificate: Option[JavaResource] = None
  protected def masterConfig: Config = ConfigFactory.empty
  protected def fileBased: Seq[FileBased]
  protected def signer: MessageSigner = DirectoryProvider.defaultSigner

  protected final def sign(fileBased: FileBased) = directoryProvider.sign(fileBased)

  override def beforeAll() = {
    super.beforeAll()
    directoryProvider
  }

  override def afterAll() = {
    closer.close()
    directoryProvider.close()
    super.afterAll()
  }
}
