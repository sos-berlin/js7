package js7.tests.testenv

import cats.effect.Resource
import com.typesafe.config.{Config, ConfigFactory}
import js7.agent.TestAgent
import js7.base.configutils.Configs.*
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WallClock
import js7.base.utils.CatsBlocking.BlockingTaskResource
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, SetOnce}
import js7.cluster.watch.ClusterWatchService
import js7.data.controller.ControllerState
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.item.{VersionId, VersionedItem, VersionedItemPath}
import js7.data.order.{OrderId, OrderObstacle, OrderObstacleCalculator}
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentDedicated}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.subagent.Subagent
import js7.tests.testenv.ControllerAgentForScalaTest.*
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
@TestOnly
trait ControllerAgentForScalaTest extends DirectoryProviderForScalaTest {
  this: org.scalatest.Suite =>

  protected final lazy val agents: Seq[TestAgent] = directoryProvider.startAgents(agentTestWiring)
    .await(99.s)
  protected final lazy val agent: TestAgent = agents.head

  protected final lazy val idToAllocatedSubagent: Map[SubagentId, Allocated[Task, Subagent]] =
    directoryProvider
      .startBareSubagents()
      .await(99.s)

  protected final lazy val controller: TestController =
    directoryProvider
      .newController(
        controllerTestWiring,
        config"""js7.web.server.auth.https-client-authentication = $controllerHttpsMutual""",
        httpPort = controllerHttpPort,
        httpsPort = controllerHttpsPort)

  protected final lazy val eventWatch = controller.eventWatch

  private val clusterWatchServiceOnce = SetOnce[Allocated[Task, ClusterWatchService]]

  protected def clusterWatchServiceResource: Option[Resource[Task, ClusterWatchService]] =
    None

  protected def controllerHttpsMutual = false

  protected def waitUntilReady = true

  protected final def controllerState: ControllerState =
    controller.controllerState()

  protected final def orderToObstacles(orderId: OrderId)(implicit clock: WallClock)
  : Checked[Set[OrderObstacle]] = {
    val service = new InstructionExecutorService(clock)
    orderObstacleCalculator.orderToObstacles(orderId)(service)
  }

  protected final def orderObstacleCalculator: OrderObstacleCalculator =
    new OrderObstacleCalculator(controllerState)

  override def beforeAll() = {
    super.beforeAll()

    idToAllocatedSubagent
    agents
    for (service <- clusterWatchServiceResource)
      clusterWatchServiceOnce := service.toAllocated.await(99.s)
    controller

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
    Task
      .parSequence(
        Seq(
          Seq(controller.terminate().void),
          clusterWatchServiceOnce.toOption.map(_.release).toList,
          agents.map(_.terminate().void),
          idToAllocatedSubagent.values.map(_.release)
        ).flatten.map(_.onErrorHandle { t =>
          logger.error(t.toStringWithCauses, t)
          throw t
        }))
      .await(99.s)

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
    idToAllocatedSubagent(subagentId).release.await(99.s)

  protected final def startBareSubagent(subagentId: SubagentId)
  : (Subagent, Task[Unit]) = {
    val subagentItem = directoryProvider.subagentItems
      .find(_.id == subagentId)
      .getOrElse(throw new NoSuchElementException(s"Missing $subagentId"))
    directoryProvider.bareSubagentResource(subagentItem, toLocalSubagentId(agentPaths.head))
      .allocated.await(99.s)
  }

  protected final def enableSubagents(subagentIdToEnable: (SubagentId, Boolean)*): Unit = {
    val eventId = eventWatch.lastAddedEventId
    controller.api
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

  protected final def runSubagent[A](
    subagentItem: SubagentItem,
    director: SubagentId = toLocalSubagentId(agentPaths.head),
    config: Config = ConfigFactory.empty,
    suffix: String = "",
    awaitDedicated: Boolean = true,
    suppressSignatureKeys: Boolean = false)
    (body: Subagent => A)
  : A =
    subagentResource(subagentItem,
      director = director,
      config = config,
      suffix = suffix,
      awaitDedicated = awaitDedicated,
      suppressSignatureKeys = suppressSignatureKeys
    ).blockingUse(99.s) { subagent =>
      // body runs in the callers test thread
      try body(subagent)
      catch {
        case NonFatal(t) =>
          logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)
          throw t
      }
    }

  protected final def subagentResource(
    subagentItem: SubagentItem,
    director: SubagentId = toLocalSubagentId(agentPaths.head),
    config: Config = ConfigFactory.empty,
    suffix: String = "",
    awaitDedicated: Boolean = true,
    suppressSignatureKeys: Boolean = false)
  : Resource[Task, Subagent] =
    Resource.suspend(Task {
      val eventId = eventWatch.lastAddedEventId
      directoryProvider
        .bareSubagentResource(subagentItem, director = director,
          config,
          suffix = suffix,
          suppressSignatureKeys = suppressSignatureKeys)
        .evalTap(_ => Task {
          if (awaitDedicated) {
            val e = eventWatch.await[SubagentDedicated](after = eventId).head.eventId
            eventWatch.await[SubagentCoupled](after = e)
          }
        })
    })
}

object ControllerAgentForScalaTest
{
  private val logger = Logger[this.type]
}
