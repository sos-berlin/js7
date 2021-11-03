package js7.tests.testenv

import cats.syntax.option._
import com.google.inject.Module
import com.google.inject.util.Modules.EMPTY_MODULE
import com.typesafe.config.{Config, ConfigFactory}
import js7.base.crypt.{DocumentSigner, SignatureVerifier, Signed, SignedString}
import js7.base.io.JavaResource
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.utils.HasCloser
import js7.base.utils.ScalaUtils.syntax._
import js7.common.message.ProblemCodeMessages
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.AgentPath
import js7.data.item.{InventoryItem, SignableItem}
import monix.execution.Scheduler
import org.scalatest.BeforeAndAfterAll
import scala.collection.immutable.Iterable

/**
  * @author Joacim Zschimmer
  */
trait DirectoryProviderForScalaTest extends BeforeAndAfterAll with HasCloser {
  this: org.scalatest.Suite =>

  coupleScribeWithSlf4j()
  ProblemCodeMessages.initialize()

  protected def commonScheduler: Option[Scheduler] =
    sys.props.contains("test.speed") ? Scheduler.global

  protected def agentPaths: Seq[AgentPath]
  protected def agentHttps = false
  protected def agentPorts: Iterable[Int] = Nil
  protected def doNotAddItems = false

  protected final lazy val directoryProvider = new DirectoryProvider(
    agentPaths,
    items = items,
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
    doNotAddItems = doNotAddItems,
    scheduler = commonScheduler)

  protected def agentConfig: Config = ConfigFactory.empty

  protected def controllerModule: Module = EMPTY_MODULE
  protected def agentModule: Module = EMPTY_MODULE
  protected lazy val controllerHttpPort = findFreeTcpPort().some
  protected lazy val controllerHttpsPort = none[Int]
  protected def agentHttpsMutual = false
  protected def provideAgentHttpsCertificate = false
  protected def provideAgentClientCertificate = false
  protected def controllerTrustStores: Iterable[JavaResource] = Nil
  protected def controllerConfig: Config = ConfigFactory.empty
  protected def items: Seq[InventoryItem]
  protected def signer: DocumentSigner = DirectoryProvider.defaultSigner
  protected def verifier: SignatureVerifier = DirectoryProvider.defaultVerifier

  protected final def sign[A <: SignableItem](item: A): Signed[A] =
    directoryProvider.sign(item)

  protected final def toSignedString[A <: SignableItem](item: A): SignedString =
    directoryProvider.toSignedString(item)

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
