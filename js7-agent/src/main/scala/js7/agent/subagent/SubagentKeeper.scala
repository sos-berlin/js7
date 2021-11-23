package js7.agent.subagent

import akka.actor.ActorSystem
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.subagent.SubagentKeeper._
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.io.process.{ProcessSignal, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
import js7.base.monixutils.{AsyncMap, AsyncVariable}
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime._
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SetOnce
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.order.OrderEvent.OrderStdWritten
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.{SubagentId, SubagentRef}
import js7.data.value.expression.Expression
import js7.journal.CommitOptions
import js7.journal.state.StatePersistence
import js7.launcher.configuration.JobLauncherConf
import js7.subagent.LocalSubagentDriver
import js7.subagent.client.{RemoteSubagentDriver, SubagentDriver}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.concurrent.Promise

final class SubagentKeeper(
  persistence: StatePersistence[AgentState],
  jobLauncherConf: JobLauncherConf,
  agentConf: AgentConfiguration,
  actorSystem: ActorSystem)
{
  private val stdoutCommitDelay = agentConf.config
    .getDuration("js7.order.stdout-stderr.commit-delay").toFiniteDuration
  private val started = SetOnce[Started]
  private val state = AsyncVariable[State](State(Map.empty, Vector.empty))
  private val orderToWaitForDriver = AsyncMap.empty[OrderId, Promise[Unit]]
  private val orderToDriver = AsyncMap.empty[OrderId, SubagentDriver]
  private val roundRobin = new RoundRobin

  def start(agentPath: AgentPath, localSubagentId: Option[SubagentId], controllerId: ControllerId)
  : Task[Unit] =
    Task.deferAction { implicit scheduler =>
      val started = Started(agentPath, localSubagentId, controllerId)
      this.started := started
      if (localSubagentId.isDefined)
        Task.unit
      else  // COMPATIBLE with v2.1 AgentRef
        state
          .update(state => Task.defer {
            val driver = newLocalSubagentDriver(compatibleLocalSubagentId, started)
            driver.start >>
              Task(state.insertDriver(driver).orThrow)
          })
          .void
    }

  private def persistStdouterr(orderId: OrderId, t: StdoutOrStderr, chunk: String): Task[Unit] =
    persistence
      // Don't wait for disk-sync.
      // OrderStdWritten is followed by a OrderProcessed, then waiting for disk-sync.
      .persistKeyedEventLater(
        orderId <-: OrderStdWritten(t)(chunk),
        options = CommitOptions(delay = stdoutCommitDelay))
      .map {
        case Left(problem) => logger.error(s"Emission of OrderStdWritten event failed: $problem")
        case Right(_) =>
      }

  def stop: Task[Unit] =
    state
      .updateWithResult(state => Task(
        state.removeAllDrivers -> state.idToDriver.values))
      .flatMap(drivers => Task
        .parSequenceUnordered(drivers.map(_.stop(Some(SIGKILL)))))
      .void

  def processOrder(order: Order[Order.Processing], defaultArguments: Map[String, Expression])
  : Task[Outcome] =
    Task.defer {
      val promise = Promise[Unit]()
      // TODO Long lock on orderId !
      orderToWaitForDriver.update(order.id, _ => Task.pure(promise)) >>
        Task
          .race(Task.fromFuture(promise.future), selectSubagentDriver)
          .guarantee(orderToWaitForDriver.remove(order.id).void)
          .map(_.toOption)
          .flatMap {
            case None => Task.pure(Outcome.Killed(Outcome.Failed(Some(
              "Order has been canceled while selecting a Subagent"))))
            case Some(driver) =>
              logger.debug(s"### ${order.id} --> ${driver.subagentId}")
              // TODO Update lock blocks the order while processing!
              orderToDriver.update(order.id, _ => Task.pure(driver)) >>
                driver
                  .processOrder(order, defaultArguments)
                  .guarantee(orderToDriver.remove(order.id).void)
          }
    }

  //TODO Fixed Priority
  private def selectSubagentDriver: Task[SubagentDriver] =
    Observable
      .repeatEval {
        val state = this.state.get
        import state.subagentIds
        subagentIds.nonEmpty ? {
          val i = roundRobin.next(subagentIds.length)
          state.idToDriver(subagentIds(i))
        }
      }
      .map(_.filter(_.isAlive))
      .delayOnNextBySelector {
        case None => Observable.unit.delayExecution(1.s/*TODO Do not poll*/)
        case Some(_) => Observable.empty
      }
      .flatMap(Observable.fromIterable(_))
      .headL

  def killProcess(orderId: OrderId, signal: ProcessSignal): Task[Unit] =
    Task.defer {
      orderToWaitForDriver
        .get(orderId)
        .fold(Task.unit)(promise => Task(promise.success(()))) >>
      orderToDriver
        .get(orderId)
        .match_ {
          case None => Task(logger.warn(s"killProcess($orderId): Unknown OrderId"))
          case Some(driver) => driver.killProcess(orderId, signal)
        }
    }

  def remove(subagentId: SubagentId): Task[Unit] =
    state
      .updateWithResult(state => Task(
        state.removeDriver(subagentId) ->
          state.idToDriver.get(subagentId)))
      .flatMap(_.fold(Task.unit)(_.stop(Some(SIGKILL))))
      .void

  def proceed(subagentRef: SubagentRef): Task[Unit] =
    started.task
      .logWhenItTakesLonger("SubagentKeeper.started?")
      .flatMap(started =>
        state
          .updateCheckedWithResult(state =>
            state.idToDriver
              .get(subagentRef.id)
              .match_ {
                case Some(existing: LocalSubagentDriver) =>
                  // TODO Subagent.uri may have changed!
                  logger.error(s"Change of local SubagentRef? $existing -> $subagentRef}")
                  //Task.pure(Right(state -> None))
                  Task.pure(Left(Problem(
                    s"Local SubagentRef (${subagentRef.id}) cannot not be changed")))

                case Some(existing: RemoteSubagentDriver[_]) =>
                  (if (subagentRef.uri == existing.subagentRef.uri)
                    Task.unit
                  else
                    existing
                      .stop(Some(SIGKILL))
                      .onErrorHandle(t => logger.error("After change of URI of " +
                        existing.subagentId + " " + existing.subagentRef.uri + ": " +
                        t.toStringWithCauses))
                      .start // Do not block the `state` here, so start and forget ???
                  ).map { _ =>
                    // Replace and forget the existing driver
                    val driver = newRemoteSubagentDriver(subagentRef, started)
                    for (updated <- state.insertDriver(driver)) yield
                      updated -> Some(driver)
                  }

                case None =>
                  Task.deferAction(implicit s => Task {
                    val subagentDriver = newSubagentDriver(subagentRef, started)
                    for (s <- state.insertDriver(subagentDriver)) yield
                      s -> Some(subagentDriver)
                  })
              })
          .map(o => o: Checked[Option[SubagentDriver]])
          .flatMapT {
            case None => Task.pure(Checked.unit)
            case Some(driver) => driver.start.map(Right(_))
          }
          .void)

  private def newSubagentDriver(subagentRef: SubagentRef, started: Started)
    (implicit scheduler: Scheduler) =
    if (started.localSubagentId contains subagentRef.id)
      newLocalSubagentDriver(subagentRef.id, started)
    else
      newRemoteSubagentDriver(subagentRef, started)

  private def newLocalSubagentDriver(subagentId: SubagentId, started: Started)
    (implicit s: Scheduler) =
    new LocalSubagentDriver(
      subagentId,
      () => persistence.currentState,
      persistStdouterr,
      started.controllerId,
      jobLauncherConf,
      SubagentDriver.Conf.fromConfig(agentConf.config))/*TODO*/

  private def newRemoteSubagentDriver(subagentRef: SubagentRef, started: Started) =
    new RemoteSubagentDriver(
      subagentRef,
      for (subagentId <- started.localSubagentId) yield
        UserAndPassword(
          UserId.checked(subagentId.string).orThrow,
          SecretString.empty),
      agentConf.httpsConfig,
      persistence,
      started.agentPath,
      started.controllerId,
      SubagentDriver.Conf.fromConfig(actorSystem.settings.config /*TODO*/),
      agentConf.recouplingStreamReaderConf,
      actorSystem)
}

object SubagentKeeper
{
  private val logger = Logger[this.type]

  // COMPATIBLE with v2.1, start automatically a local Subagent
  private val compatibleLocalSubagentId = SubagentId("Local")

  private case class Started(
    agentPath: AgentPath,
    localSubagentId: Option[SubagentId],
    controllerId: ControllerId)

  private final case class State(
    idToDriver: Map[SubagentId, SubagentDriver],
    subagentIds: IndexedSeq[SubagentId])
  {
    def insertDriver(driver: SubagentDriver): Checked[State] =
      for (o <- idToDriver.insert(driver.subagentId -> driver))
        yield copy(
          idToDriver = o,
          subagentIds = subagentIds :+ driver.subagentId)

    def removeDriver(subagentId: SubagentId): State =
      copy(
        idToDriver = idToDriver.removed(subagentId),
        subagentIds = subagentIds.filter(_ != subagentId))

    def removeAllDrivers: State =
      copy(
        idToDriver = Map.empty,
        subagentIds = Vector.empty)
  }
}
