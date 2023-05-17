package js7.tests.testenv

import akka.actor.ActorSystem
import com.typesafe.config.Config
import js7.base.auth.Admission
import js7.base.eventbus.StandardEventBus
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichEitherF}
import js7.base.utils.{Allocated, Lazy, ProgramTermination}
import js7.base.web.Uri
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResource
import js7.controller.configuration.ControllerConfiguration
import js7.controller.problems.ControllerIsShuttingDownProblem
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
import js7.tests.testenv.TestController.*
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.Future
import scala.util.{Failure, Success}

final class TestController(allocated: Allocated[Task, RunningController], admission: Admission)
{
  val runningController: RunningController =
    allocated.allocatedThing

  val conf: ControllerConfiguration =
    runningController.conf

  private val apiLazy = Lazy(new ControllerApi(
    admissionsToApiResource(Nel.one(admission))(actorSystem),
    failWhenUnreachable = true))

  implicit lazy val api: ControllerApi =
    apiLazy.value


  val stop: Task[Unit] =
    stopControllerApi
      .guarantee(allocated.stop)
      .memoize

  private def stopControllerApi: Task[Unit] =
    Task.defer(apiLazy.toOption.fold(Task.unit)(controllerApi =>
      controllerApi
        .stop(dontLogout = true/*Akka may block when server has just been shut down*/)
        .logWhenItTakesLonger))

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

  def controllerState(): ControllerState = {
    import runningController.scheduler
    runningController.controllerState.await(99.s)
  }

  def sessionRegister: SessionRegister[SimpleSession] =
    runningController.sessionRegister

  def actorSystem: ActorSystem =
    runningController.actorSystem

  def testEventBus: StandardEventBus[Any] =
    runningController.testEventBus

  def terminated: Future[ProgramTermination] =
    runningController.terminated

  def untilTerminated: Task[ProgramTermination] =
    runningController.untilTerminated.logWhenItTakesLonger

  def terminate(
    suppressSnapshot: Boolean = false,
    clusterAction: Option[ShutDown.ClusterAction] = None,
    dontNotifyActiveNode: Boolean = false)
  : Task[ProgramTermination] =
    logger.traceTask {
      val cmd = ShutDown(
        suppressSnapshot = suppressSnapshot,
        clusterAction = clusterAction,
        dontNotifyActiveNode = dontNotifyActiveNode)

      shutdown(cmd).guarantee(stop)
    }

  private def shutdown(cmd: ShutDown): Task[ProgramTermination] =
    logger.debugTask(Task.defer {
      if (terminated.isCompleted) // Works only if previous termination has been completed
        untilTerminated
      else
        actorSystem.whenTerminated.value match {
          case Some(Failure(t)) => Task.raiseError(t)
          case Some(Success(_)) =>
            logger.warn("Controller terminate: Akka has already been terminated")
            Task.pure(ProgramTermination())
          case None =>
            api
              .executeCommand(cmd)
              .rightAs(())
              .flatMapLeftCase { case problem @ ControllerIsShuttingDownProblem =>
                logger.info(problem.toString)
                Task.right(())
              }
              .map(_.orThrow)
              .*>(untilTerminated)
        }
    }).logWhenItTakesLonger

  def updateItemsAsSystemUser(operations: Observable[ItemOperation]): Task[Checked[Completed]] =
    runningController.updateItemsAsSystemUser(operations)

  def addOrderBlocking(order: FreshOrder): Unit = {
    import runningController.scheduler
    runningController.addOrder(order).await(99.s).orThrow
  }

  def runOrder(order: FreshOrder): Seq[Stamped[OrderEvent]] = {
    import runningController.scheduler
    val timeout = 99.s
    logger.debugTask("runOrder", order.id)(Task.defer {
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
        .logWhenItTakesLonger(s"runOrder(${order.id})")
    }).await(timeout)
  }

  def waitUntilReady(): Unit =
    runningController.waitUntilReady()

  def clusterState: Task[ClusterState] =
    runningController.clusterState

  def journalActorState: JournalActor.Output.JournalActorState =
    runningController.journalActorState

  override def toString = s"TestController(${conf.controllerId}/${conf.clusterConf.ownId})"
}

object TestController {
  private val logger = Logger[this.type]
}
