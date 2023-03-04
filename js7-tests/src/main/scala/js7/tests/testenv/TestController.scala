package js7.tests.testenv

import akka.actor.ActorSystem
import com.google.inject.Injector
import com.typesafe.config.Config
import js7.base.auth.{SimpleUser, UserAndPassword}
import js7.base.eventbus.StandardEventBus
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.{Allocated, ProgramTermination}
import js7.base.web.Uri
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.controller.client.HttpControllerApi
import js7.controller.configuration.ControllerConfiguration
import js7.controller.{OrderApi, RunningController}
import js7.core.command.CommandMeta
import js7.data.cluster.ClusterState
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.controller.{ControllerCommand, ControllerState}
import js7.data.event.{EventId, EventRequest, Stamped}
import js7.data.item.{ItemOperation, UnsignedSimpleItem}
import js7.data.order.OrderEvent.{OrderDeleted, OrderFailed, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent}
import js7.journal.JournalActor
import js7.journal.watch.StrictEventWatch
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Future

@TestOnly
final class TestController(allocated: Allocated[Task, RunningController])
extends AutoCloseable
{
  def runningController: RunningController =
    allocated.allocatedThing

  implicit val scheduler: Scheduler =
    runningController.scheduler

  def close() =
    stop.await(99.s)

  def stop: Task[Unit] =
    allocated.stop

  val conf: ControllerConfiguration =
    runningController.conf

  val config: Config =
    conf.config

  def localUri: Uri =
    runningController.localUri

  def httpApi: HttpControllerApi =
    runningController.httpApi

  val eventWatch: StrictEventWatch =
    runningController.eventWatch

  def recoveredEventId: EventId =
    runningController.recoveredEventId

  def orderApi: OrderApi =
    runningController.orderApi

  def controllerState: Task[ControllerState] =
    runningController.controllerState

  def sessionRegister: SessionRegister[SimpleSession] =
    runningController.sessionRegister

  def actorSystem: ActorSystem =
    runningController.actorSystem

  def testEventBus: StandardEventBus[Any] =
    runningController.testEventBus

  def injector: Injector =
    runningController.injector

  def terminated: Future[ProgramTermination] =
    runningController.terminated

  def terminate(
    suppressSnapshot: Boolean = false,
    clusterAction: Option[ShutDown.ClusterAction] = None,
    dontNotifyActiveNode: Boolean = false)
  : Task[ProgramTermination] =
    runningController
      .terminate(
        suppressSnapshot = suppressSnapshot,
        clusterAction,
        dontNotifyActiveNode = dontNotifyActiveNode)
      .<*(allocated.stop)

  def executeCommandForTest(command: ControllerCommand): Checked[command.Response] =
    executeCommandAsSystemUser(command) await 99.s

  def executeCommandAsSystemUser(command: ControllerCommand): Task[Checked[command.Response]] =
    runningController.executeCommandAsSystemUser(command)

  def executeCommand(command: ControllerCommand, meta: CommandMeta)
  : Task[Checked[command.Response]] =
    runningController.executeCommand(command, meta)

  def updateUnsignedSimpleItemsAsSystemUser(items: Seq[UnsignedSimpleItem])
  : Task[Checked[Completed]] =
    runningController.updateUnsignedSimpleItemsAsSystemUser(items)

  def updateUnsignedSimpleItems(user: SimpleUser, items: Seq[UnsignedSimpleItem])
  : Task[Checked[Completed]] =
    runningController.updateUnsignedSimpleItems(user, items)

  def updateItemsAsSystemUser(operations: Observable[ItemOperation]): Task[Checked[Completed]] =
    runningController.updateItemsAsSystemUser(operations)

  def updateItems(user: SimpleUser, operations: Observable[ItemOperation])
  : Task[Checked[Completed]] =
    runningController.updateItems(user, operations)

  def addOrderBlocking(order: FreshOrder): Unit =
    runningController.addOrderBlocking((order))

  def addOrder(order: FreshOrder): Task[Checked[Unit]] =
    runningController.addOrder(order)

  def runOrder(order: FreshOrder): Seq[Stamped[OrderEvent]] = {
    val timeout = 99.s
    val eventId = eventWatch.lastAddedEventId
    addOrderBlocking(order)
    eventWatch
      .observe(EventRequest.singleClass[OrderEvent](eventId, Some(timeout + 9.s)))
      .filter(_.value.key == order.id)
      .map(o => o.copy(value = o.value.event))
      .takeWhileInclusive { case Stamped(_, _, event) =>
        if (order.deleteWhenTerminated)
          event != OrderDeleted && !event.isInstanceOf[OrderFailed]
        else
          !event.isInstanceOf[OrderTerminated]
      }
      .toL(Vector)
      .executeOn(scheduler)
      .await(timeout)
  }

  def waitUntilReady(): Unit =
    runningController.waitUntilReady()

  def clusterState: Task[ClusterState] =
    runningController.clusterState

  def httpApiDefaultLogin(userAndPassword: Option[UserAndPassword]): Unit =
    runningController.httpApiDefaultLogin(userAndPassword)

  def journalActorState: JournalActor.Output.JournalActorState =
    runningController.journalActorState
}
