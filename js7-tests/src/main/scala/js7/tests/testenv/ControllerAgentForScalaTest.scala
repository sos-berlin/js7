package js7.tests.testenv

import cats.syntax.traverse.*
import js7.agent.RunningAgent
import js7.base.configutils.Configs.*
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WallClock
import js7.base.utils.Lazy
import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.RunningController
import js7.data.controller.ControllerState
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.item.{VersionId, VersionedItem, VersionedItemPath}
import js7.data.order.{OrderId, OrderObstacle, OrderObstacleCalculator}
import js7.data.subagent.SubagentItemStateEvent.SubagentCoupled
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.subagent.BareSubagent
import js7.tests.testenv.ControllerAgentForScalaTest.*
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
trait ControllerAgentForScalaTest extends DirectoryProviderForScalaTest {
  this: org.scalatest.Suite =>

  protected final lazy val agents: Seq[RunningAgent] = directoryProvider.startAgents(agentModule)
    .await(99.s)
  protected final lazy val agent: RunningAgent = agents.head

  protected final lazy val (bareSubagents, bareSubagentIdToRelease)
  : (Seq[BareSubagent], Map[SubagentId, Task[Unit]]) =
    directoryProvider
      .startBareSubagents()
      .map(pairs => pairs.map(_._1) -> pairs.map(_._2).toMap)
      .await(99.s)

  protected final lazy val controller: RunningController =
    directoryProvider
      .startController(
        controllerModule,
        config"""js7.web.server.auth.https-client-authentication = $controllerHttpsMutual""",
        httpPort = controllerHttpPort,
        httpsPort = controllerHttpsPort)
      .tapEval(controller =>
        Task(controller.httpApiDefaultLogin(Some(directoryProvider.controller.userAndPassword))))
      .await(99.s)

  protected final lazy val eventWatch = controller.eventWatch
  private val controllerApiLazy = Lazy(directoryProvider.newControllerApi(controller))
  protected lazy val controllerApi = controllerApiLazy()

  protected def controllerHttpsMutual = false

  protected def waitUntilReady = true

  protected final def controllerState: ControllerState =
    controller.controllerState.await(99.s)

  protected final def orderToObstacles(orderId: OrderId)(implicit clock: WallClock)
  : Checked[Set[OrderObstacle]] = {
    val service = new InstructionExecutorService(clock)
    orderObstacleCalculator.orderToObstacles(orderId)(service)
  }

  protected final def orderObstacleCalculator: OrderObstacleCalculator =
    new OrderObstacleCalculator(controllerState)

  override def beforeAll() = {
    super.beforeAll()
    bareSubagents
    agents
    if (waitUntilReady) {
      controller.waitUntilReady()
      if (!doNotAddItems) {
        for (subagentItem <- directoryProvider.subagentItems) {
          eventWatch.await[SubagentCoupled](_.key == subagentItem.id)
        }
      }
    }
  }

  override def afterAll() = {
    for (o <- controllerApiLazy) o.stop await 99.s
    controller.terminate() await 15.s
    controller.close()
    agents.traverse(a => a.terminate() >> Task(a.close())) await 15.s

    try bareSubagentIdToRelease.values.await(99.s)
    catch { case NonFatal(t) =>
      logger.error(s"bareSubagentIdToRelease => ${t.toStringWithCauses}", t)
    }

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

  protected final def stopBareSubagent(subagentId: SubagentId): Unit =
    bareSubagentIdToRelease(subagentId).await(99.s)

  protected final def startBareSubagent(subagentId: SubagentId): (BareSubagent, Task[Unit]) = {
    val subagentItem = directoryProvider.subagentItems
      .find(_.id == subagentId)
      .getOrElse(throw new NoSuchElementException(s"Missing $subagentId"))
    directoryProvider.subagentResource(subagentItem).allocated.await(99.s)
  }

  protected final def enableSubagents(subagentIdToEnable: (SubagentId, Boolean)*): Unit = {
    val eventId = eventWatch.lastAddedEventId
    controllerApi
      .updateItems(Observable
        .fromIterable(subagentIdToEnable)
        .map {
          case (subagentId, enable) =>
            val subagentItem = controllerState.keyToItem(SubagentItem)(subagentId)
            AddOrChangeSimple(subagentItem.withRevision(None).copy(disabled = !enable))
        })
      .await(99.s).orThrow
    for (subagentId <- subagentIdToEnable.map(_._1)) {
      eventWatch.await[ItemAttached](_.event.key == subagentId, after = eventId)
    }
  }
}

object ControllerAgentForScalaTest
{
  private val logger = Logger[this.type]
}
