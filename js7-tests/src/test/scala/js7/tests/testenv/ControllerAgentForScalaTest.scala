package js7.tests.testenv

import cats.syntax.traverse._
import js7.agent.RunningAgent
import js7.base.auth.Admission
import js7.base.configutils.Configs._
import js7.base.thread.Futures.implicits._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.controller.RunningController
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResources
import js7.data.item.{VersionId, VersionedItem, VersionedItemPath}
import js7.proxy.ControllerApi
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
trait ControllerAgentForScalaTest extends DirectoryProviderForScalaTest {
  this: org.scalatest.Suite =>

  protected def commonScheduler: Option[Scheduler] = None

  protected final lazy val agents: Seq[RunningAgent] = directoryProvider.startAgents() await 99.s
  protected final lazy val agent: RunningAgent = agents.head

  protected final lazy val controller: RunningController =
    directoryProvider
      .startController(
        controllerModule,
        config"""js7.web.server.auth.https-client-authentication = $controllerHttpsMutual""",
        httpPort = controllerHttpPort,
        httpsPort = controllerHttpsPort,
        scheduler = commonScheduler)
      .tapEval(controller =>
        Task(controller.httpApiDefaultLogin(Some(directoryProvider.controller.userAndPassword))))
      .await(99.s)

  protected final lazy val eventWatch = controller.eventWatch
  protected lazy val controllerAdmission = Admission(
    controller.localUri,
    Some(directoryProvider.controller.userAndPassword))
  protected lazy val controllerApi = new ControllerApi(
    admissionsToApiResources(Seq(controllerAdmission))(controller.actorSystem))

  protected def controllerHttpsMutual = false

  protected def waitUntilReady = true

  override def beforeAll() = {
    super.beforeAll()
    agents
    if (waitUntilReady) {
      controller.waitUntilReady()
    }
  }

  override def afterAll() = {
    controllerApi.stop await 99.s
    controller.terminate() await 15.s
    controller.close()
    agents.traverse(a => a.terminate() >> Task(a.close())) await 15.s
    super.afterAll()
  }

  private val usedVersionIds = mutable.Set[VersionId]()

  final def updateVersionedItems(change: Seq[VersionedItem]): VersionId =
    updateVersionedItems(change, Nil)

  final def updateVersionedItems(
    change: Seq[VersionedItem],
    delete: Seq[VersionedItemPath])
  : VersionId = {
    val versionId = VersionId.generate(usedVersionIds)
    updateVersionedItems(versionId, change, delete)
    versionId
  }

  final def updateVersionedItems(
    versionId: VersionId,
    change: Seq[VersionedItem] = Nil,
    delete: Seq[VersionedItemPath] = Nil)
  : Unit = {
    usedVersionIds += versionId
    directoryProvider.updateVersionedItems(controller, versionId, change, delete)
  }
}
