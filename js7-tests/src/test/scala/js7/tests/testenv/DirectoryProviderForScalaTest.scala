package js7.tests.testenv

import cats.syntax.option.*
import com.typesafe.config.{Config, ConfigFactory}
import js7.agent.RunningAgent
import js7.base.auth.Admission
import js7.base.crypt.{DocumentSigner, SignatureVerifier, Signed, SignedString}
import js7.base.io.JavaResource
import js7.base.test.TestCatsEffect
import js7.base.utils.HasCloser
import js7.base.web.Uri
import js7.common.message.ProblemCodeMessages
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.RunningController
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.item.{InventoryItem, SignableItem}
import js7.data.subagent.SubagentItem
import js7.subagent.Subagent
import org.jetbrains.annotations.TestOnly
import org.scalatest.BeforeAndAfterAll
import scala.collection.immutable.Iterable

/**
  * @author Joacim Zschimmer
  */
@TestOnly
trait DirectoryProviderForScalaTest extends BeforeAndAfterAll, TestCatsEffect, HasCloser:
  this: org.scalatest.Suite =>

  ProblemCodeMessages.initialize()

  // UNUSED
  protected final def commonScheduler: Nothing = sys.error("commonScheduler?")

  protected def agentPaths: Seq[AgentPath]
  protected def agentHttps = false
  protected def agentPorts: Seq[Int] = Nil
  protected def directorEnvToToAgentRef(directorEnv: DirectorEnv): AgentRef =
    AgentRef(directorEnv.agentPath, Vector(directorEnv.localSubagentId))


  protected def primarySubagentsDisabled = false
  protected def bareSubagentItems: Seq[SubagentItem] = Nil

  protected final lazy val directoryProvider = new DirectoryProvider(
    agentPaths = agentPaths,
    bareSubagentItems = bareSubagentItems,
    items = items,
    controllerConfig = controllerConfig,
    agentHttps = agentHttps,
    agentHttpsMutual = agentHttpsMutual,
    agentConfig = agentConfig,
    agentPorts = agentPorts,
    directorEnvToAgentRef = directorEnvToToAgentRef,
    primarySubagentsDisabled = primarySubagentsDisabled,
    provideAgentHttpsCertificate = provideAgentHttpsCertificate,
    provideAgentClientCertificate = provideAgentClientCertificate,
    controllerTrustStores = controllerTrustStores,
    signer = signer,
    verifier = verifier,
    testName = Some(getClass.getSimpleName),
    doNotAddItems = doNotAddItems)

  protected def agentConfig: Config = ConfigFactory.empty

  protected def controllerTestWiring: RunningController.TestWiring =
    RunningController.TestWiring.empty
  protected def agentTestWiring: RunningAgent.TestWiring = RunningAgent.TestWiring.empty
  protected def subagentTestWiring: Subagent.TestWiring = Subagent.TestWiring.empty
  protected lazy val controllerHttpPort = findFreeTcpPort().some
  protected lazy val controllerHttpsPort = none[Int]
  protected lazy val controllerAdmission = Admission(
    Uri(controllerHttpPort
      .map(port => s"http://127.0.0.1:$port")
      .getOrElse(s"https://localhost:${controllerHttpsPort.get}")),
    Some(directoryProvider.controllerEnv.userAndPassword))
  protected def agentHttpsMutual = false
  protected def provideAgentHttpsCertificate = false
  protected def provideAgentClientCertificate = false
  protected def controllerTrustStores: Iterable[JavaResource] = Nil
  protected def controllerConfig: Config = ConfigFactory.empty
  protected def items: Seq[InventoryItem]
  protected def doNotAddItems = false
  protected def signer: DocumentSigner = DirectoryProvider.defaultSigner
  protected def verifier: SignatureVerifier = DirectoryProvider.defaultVerifier

  protected final def sign[A <: SignableItem](item: A): Signed[A] =
    directoryProvider.sign(item)

  protected final def toSignedString[A <: SignableItem](item: A): SignedString =
    directoryProvider.toSignedString(item)

  protected final lazy val blockingItemUpdater = new BlockingItemUpdater:
    def sign[A <: SignableItem](item: A) =
      DirectoryProviderForScalaTest.this.sign(item)

  export blockingItemUpdater.{deleteItems, nextVersionId, updateItem, updateItems, withItem, withItems}

  override def beforeAll() =
    super.beforeAll()
    directoryProvider

  override def afterAll() =
    try
      closer.close()
      directoryProvider.close()
    finally
      super.afterAll()
