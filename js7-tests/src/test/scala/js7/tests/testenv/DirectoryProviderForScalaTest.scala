package js7.tests.testenv

import cats.syntax.option._
import com.google.inject.Module
import com.google.inject.util.Modules.EMPTY_MODULE
import com.typesafe.config.{Config, ConfigFactory}
import js7.base.crypt.MessageSigner
import js7.base.utils.HasCloser
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.message.ProblemCodeMessages
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.common.utils.JavaResource
import js7.data.agent.AgentRefPath
import js7.data.item.InventoryItem
import org.scalatest.BeforeAndAfterAll
import scala.collection.immutable.Iterable

/**
  * @author Joacim Zschimmer
  */
trait DirectoryProviderForScalaTest extends BeforeAndAfterAll with HasCloser {
  this: org.scalatest.Suite =>

  coupleScribeWithSlf4j()
  ProblemCodeMessages.initialize()

  protected def agentRefPaths: Seq[AgentRefPath]
  protected def agentHttps = false
  protected def agentPorts: Iterable[Int] = Nil
  protected def suppressRepoInitialization = false

  protected final lazy val directoryProvider = new DirectoryProvider(
    agentRefPaths,
    inventoryItems = inventoryItems,
    controllerConfig = controllerConfig,
    agentHttps = agentHttps,
    agentHttpsMutual = agentHttpsMutual,
    agentConfig = agentConfig,
    agentPorts = agentPorts,
    provideAgentHttpsCertificate = provideAgentHttpsCertificate,
    provideAgentClientCertificate = provideAgentClientCertificate,
    controllerTrustStores = controllerTrustStores,
    signer = signer,
    testName = Some(getClass.getSimpleName),
    suppressRepo = suppressRepoInitialization)

  protected def agentConfig: Config = ConfigFactory.empty

  protected val controllerModule: Module = EMPTY_MODULE
  protected lazy val controllerHttpPort = findFreeTcpPort().some
  protected lazy val controllerHttpsPort = none[Int]
  protected def agentHttpsMutual = false
  protected def provideAgentHttpsCertificate = false
  protected def provideAgentClientCertificate = false
  protected def controllerTrustStores: Iterable[JavaResource] = Nil
  protected def controllerConfig: Config = ConfigFactory.empty
  protected def inventoryItems: Seq[InventoryItem]
  protected def signer: MessageSigner = DirectoryProvider.defaultSigner

  protected final def toSigned(item: InventoryItem) = directoryProvider.toSigned(item)
  protected final def sign(item: InventoryItem) = directoryProvider.sign(item)

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
