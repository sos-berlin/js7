package js7.tests.testenv

import cats.syntax.option._
import com.google.inject.Module
import com.google.inject.util.Modules.EMPTY_MODULE
import com.typesafe.config.{Config, ConfigFactory}
import js7.base.crypt.{DocumentSigner, SignatureVerifier}
import js7.base.utils.HasCloser
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.message.ProblemCodeMessages
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.common.utils.JavaResource
import js7.data.agent.AgentId
import js7.data.item.VersionedItem
import org.scalatest.BeforeAndAfterAll
import scala.collection.immutable.Iterable

/**
  * @author Joacim Zschimmer
  */
trait DirectoryProviderForScalaTest extends BeforeAndAfterAll with HasCloser {
  this: org.scalatest.Suite =>

  coupleScribeWithSlf4j()
  ProblemCodeMessages.initialize()

  protected def agentIds: Seq[AgentId]
  protected def agentHttps = false
  protected def agentPorts: Iterable[Int] = Nil
  protected def doNotAddItems = false

  protected final lazy val directoryProvider = new DirectoryProvider(
    agentIds,
    versionedItems = versionedItems,
    controllerConfig = controllerConfig,
    agentHttps = agentHttps,
    agentHttpsMutual = agentHttpsMutual,
    agentConfig = agentConfig,
    agentPorts = agentPorts,
    provideAgentHttpsCertificate = provideAgentHttpsCertificate,
    provideAgentClientCertificate = provideAgentClientCertificate,
    controllerTrustStores = controllerTrustStores,
    signer = signer,
    verifier = verifier,
    testName = Some(getClass.getSimpleName),
    doNotAddItems = doNotAddItems)

  protected def agentConfig: Config = ConfigFactory.empty

  protected val controllerModule: Module = EMPTY_MODULE
  protected lazy val controllerHttpPort = findFreeTcpPort().some
  protected lazy val controllerHttpsPort = none[Int]
  protected def agentHttpsMutual = false
  protected def provideAgentHttpsCertificate = false
  protected def provideAgentClientCertificate = false
  protected def controllerTrustStores: Iterable[JavaResource] = Nil
  protected def controllerConfig: Config = ConfigFactory.empty
  protected def versionedItems: Seq[VersionedItem]
  protected def signer: DocumentSigner = DirectoryProvider.defaultSigner
  protected def verifier: SignatureVerifier = DirectoryProvider.defaultVerifier

  protected final def toSigned(item: VersionedItem) = directoryProvider.toSigned(item)
  protected final def sign(item: VersionedItem) = directoryProvider.sign(item)

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
