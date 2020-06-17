package js7.tests.testenv

import com.google.inject.Module
import com.google.inject.util.Modules.EMPTY_MODULE
import com.typesafe.config.{Config, ConfigFactory}
import js7.base.crypt.MessageSigner
import js7.base.utils.HasCloser
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.common.utils.JavaResource
import js7.core.message.ProblemCodeMessages
import js7.data.agent.AgentRefPath
import js7.data.filebased.FileBased
import org.scalatest.BeforeAndAfterAll

/**
  * @author Joacim Zschimmer
  */
trait DirectoryProviderForScalaTest extends BeforeAndAfterAll with HasCloser {
  this: org.scalatest.Suite =>

  ProblemCodeMessages.initialize()

  protected def agentRefPaths: Seq[AgentRefPath]
  protected def agentHttps = false
  protected def agentPorts: Iterable[Int] = Nil
  protected def suppressRepoInitialization = false

  protected final lazy val directoryProvider = new DirectoryProvider(
    agentRefPaths,
    fileBased = fileBased,
    masterConfig = masterConfig,
    agentHttps = agentHttps, agentHttpsMutual = agentHttpsMutual,
    agentConfig = agentConfig,
    agentPorts = agentPorts,
    provideAgentHttpsCertificate = provideAgentHttpsCertificate,
    provideAgentClientCertificate = provideAgentClientCertificate,
    masterHttpsMutual = masterHttpsMutual,
    masterClientCertificate = masterClientCertificate,
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
