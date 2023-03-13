package js7.tests.testenv

import akka.actor.ActorSystem
import com.typesafe.config.Config
import js7.base.auth.Admission
import js7.base.eventbus.StandardEventBus
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.utils.{Allocated, Lazy, ProgramTermination}
import js7.base.web.Uri
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResources
import js7.controller.configuration.ControllerConfiguration
import js7.controller.{OrderApi, RunningController}
import js7.data.cluster.ClusterState
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.controller.ControllerState
import js7.data.event.{EventId, EventRequest, Stamped}
import js7.data.item.ItemOperation
import js7.data.order.OrderEvent.{OrderDeleted, OrderFailed, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent}
import js7.journal.JournalActor
import js7.journal.watch.StrictEventWatch
import js7.proxy.ControllerApi
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.concurrent.Future

final class TestController(allocated: Allocated[Task, RunningController], admission: Admission)
extends AutoCloseable
{
  val conf: ControllerConfiguration =
    runningController.conf

  private val apiLazy = Lazy(new ControllerApi(
    admissionsToApiResources(Nel.one(admission))(actorSystem),
    failWhenUnreachable = true))

  implicit lazy val api: ControllerApi =
    apiLazy.value

  def runningController: RunningController =
    allocated.allocatedThing

  implicit val scheduler: Scheduler =
    runningController.scheduler

  def close() =
    stop.await(99.s)

  def stop: Task[Unit] =
    stopControllerApi
      .guarantee(allocated.stop)

  private def stopControllerApi: Task[Unit] =
    Task.defer(apiLazy.toOption.fold(Task.unit)(controllerApi =>
      controllerApi.stop(dontLogout = true/*Akka may block when server has been shut down*/)))

  def config: Config =
    conf.config

  def localUri: Uri =
    runningController.localUri

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
      .guarantee(allocated.stop)

  def updateItemsAsSystemUser(operations: Observable[ItemOperation]): Task[Checked[Completed]] =
    runningController.updateItemsAsSystemUser(operations)

  def addOrderBlocking(order: FreshOrder): Unit =
    runningController.addOrder(order).await(99.s).orThrow

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

  def journalActorState: JournalActor.Output.JournalActorState =
    runningController.journalActorState
}
