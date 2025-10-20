package js7.tests.testenv

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.typesafe.config.Config
import fs2.Stream
import izumi.reflect.Tag
import js7.base.auth.Admission
import js7.base.catsutils.CatsEffectExtensions.right
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.eventbus.StandardEventBus
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked
import js7.base.scalasource.ScalaSourceLocation
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.CatsUtils.syntax.{logWhenItTakesLonger, logWhenMethodTakesLonger}
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichEitherF}
import js7.base.utils.{Allocated, Lazy, ProgramTermination}
import js7.base.web.Uri
import js7.cluster.watch.ClusterWatch
import js7.common.pekkohttp.web.session.{SessionRegister, SimpleSession}
import js7.controller.client.PekkoHttpControllerApi.admissionsToApiResource
import js7.controller.configuration.ControllerConfiguration
import js7.controller.problems.ControllerIsShuttingDownProblem
import js7.controller.{OrderApi, RunningController}
import js7.data.agent.AgentPath
import js7.data.cluster.ClusterState
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.controller.{ControllerCommand, ControllerState}
import js7.data.event.{EventId, EventRequest, Stamped}
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.ItemOperation
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.order.OrderEvent.{OrderDeleted, OrderFailed, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.journal.JournalActor
import js7.journal.watch.StrictEventWatch
import js7.proxy.ControllerApi
import js7.tests.testenv.TestController.*
import org.apache.pekko.actor.ActorSystem
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

final class TestController(allocated: Allocated[IO, RunningController], admission: Admission):
  val runningController: RunningController =
    allocated.allocatedThing

  val conf: ControllerConfiguration =
    runningController.conf

  private val apiLazy = Lazy(new ControllerApi(
    admissionsToApiResource(Nel.one(admission))(using actorSystem),
    failWhenUnreachable = true))

  implicit lazy val api: ControllerApi =
    apiLazy.value

  val stop: IO[Unit] =
    stopControllerApi
      .guarantee(allocated.release)
      .unsafeMemoize

  private def stopControllerApi: IO[Unit] =
    IO.defer(apiLazy.toOption.fold(IO.unit): controllerApi =>
      controllerApi
        .stop(dontLogout = true/*Pekko may block when server has just been shut down*/)
        .logWhenMethodTakesLonger)

  def config: Config =
    conf.config

  def localUri: Uri =
    runningController.localUri

  val eventWatch: StrictEventWatch =
    runningController.eventWatch

  export eventWatch.{await, awaitKey, awaitKeys, awaitNext, awaitNextKey, eventsByKey, expect, keyedEvents, lastAddedEventId, resetLastWatchedEventId}

  def recoveredEventId: EventId =
    runningController.recoveredEventId

  def orderApi: OrderApi =
    runningController.orderApi

  def controllerState(): ControllerState =
    import runningController.ioRuntime
    runningController.controllerState.await(99.s)

  def sessionRegister: SessionRegister[SimpleSession] =
    runningController.sessionRegister

  def actorSystem: ActorSystem =
    runningController.actorSystem

  def testEventBus: StandardEventBus[Any] =
    runningController.testEventBus

  def terminated: Future[ProgramTermination] =
    runningController.terminated

  def untilTerminated: IO[ProgramTermination] =
    runningController.untilTerminated.logWhenMethodTakesLonger

  def terminate(
    suppressSnapshot: Boolean = false,
    clusterAction: Option[ShutDown.ClusterAction] = None,
    dontNotifyActiveNode: Boolean = false)
  : IO[ProgramTermination] =
    logger.traceIO(
      shutdown(ShutDown(
        suppressSnapshot = suppressSnapshot,
        clusterAction = clusterAction,
        dontNotifyActiveNode = dontNotifyActiveNode)
      ).guarantee(stop))

  def execCmd[C <: ControllerCommand](command: C)(using Tag[command.Response]): command.Response =
    import runningController.ioRuntime
    api.executeCommand(command).await(99.s).orThrow

  private def shutdown(cmd: ShutDown): IO[ProgramTermination] =
    logger.debugIO(IO.defer {
      if terminated.isCompleted then // Works only if previous termination has been completed
        untilTerminated
      else
        actorSystem.whenTerminated.value match {
          case Some(Failure(t)) => IO.raiseError(t)
          case Some(Success(_)) =>
            logger.warn("Controller terminate: Pekko has already been terminated")
            IO.pure(ProgramTermination())
          case None =>
            api
              .executeCommand(cmd)
              .rightAs(())
              .flatMapLeftCase { case problem @ ControllerIsShuttingDownProblem =>
                logger.info(problem.toString)
                IO.right(())
              }
              .map(_.orThrow)
              .*>(untilTerminated)
        }
    }).logWhenMethodTakesLonger

  def updateItemsAsSystemUser(operations: Stream[IO, ItemOperation]): IO[Checked[Completed]] =
    runningController.updateItemsAsSystemUser(operations)

  def addOrderBlocking(order: FreshOrder): Unit =
    import runningController.ioRuntime
    runningController.addOrder(order).await(99.s).orThrow

  def runOrder(order: FreshOrder, timeout: FiniteDuration = 99.s): Seq[Stamped[OrderEvent]] =
    import runningController.ioRuntime
    logger.debugIO("runOrder", order.id)(IO.defer {
      val eventId = eventWatch.lastAddedEventId
      addOrderBlocking(order)
      eventWatch
        .stream(EventRequest.singleClass[OrderEvent](eventId, Some(timeout + 9.s)))
        .filter(_.value.key == order.id)
        .map(o => o.copy(value = o.value.event))
        .takeThrough { case Stamped(_, _, event) =>
          if order.deleteWhenTerminated then
            event != OrderDeleted && !event.isInstanceOf[OrderFailed]
          else
            !event.isInstanceOf[OrderTerminated]
        }
        .compile
        .toVector
        .logWhenItTakesLonger(s"runOrder(${order.id})")
    }).await(timeout)

  def enableSubagents(subagentIdToEnable: (SubagentId, Boolean)*)(using IORuntime): Unit =
    val eventId = lastAddedEventId
    val controllerState = this.controllerState()
    api.updateItems:
      Stream.iterable(subagentIdToEnable)
        .map: (subagentId, enable) =>
          val subagentItem = controllerState.keyToItem(SubagentItem)(subagentId)
          AddOrChangeSimple(subagentItem.withRevision(None).copy(disabled = !enable))
    .await(99.s).orThrow
    for subagentId <- subagentIdToEnable.map(_._1) do
      awaitNext[ItemAttached](_.event.key == subagentId, after = eventId)

  def untilReady: IO[Unit] =
    runningController.untilReady

  def waitUntilReady(): Unit =
    runningController.waitUntilReady()

  def clusterState: IO[ClusterState] =
    runningController.clusterState

  def journalActorState: JournalActor.Output.JournalActorState =
    runningController.journalActorState

  def clusterWatchFor(agentPath: AgentPath): Checked[ClusterWatch] =
    import runningController.ioRuntime
    runningController.clusterWatchServiceFor((agentPath))
      .await(99.s)
      .map(_.clusterWatch)

  override def toString = s"TestController(${conf.controllerId}/${conf.clusterConf.ownId})"


object TestController:
  private val logger = Logger[this.type]
