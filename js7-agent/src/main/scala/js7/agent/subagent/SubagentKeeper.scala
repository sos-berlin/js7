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
import js7.data.execution.workflow.instructions.ScheduleSimulator.Scheduled
import js7.data.order.OrderEvent.{LegacySubagentId, OrderCoreEvent, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten}
import js7.data.order.{Order, OrderId}
import js7.data.subagent.{SubagentId, SubagentRef, SubagentRefState}
import js7.data.value.expression.Expression
import js7.journal.CommitOptions
import js7.journal.state.StatePersistence
import js7.launcher.configuration.JobLauncherConf
import js7.subagent.LocalSubagentDriver
import js7.subagent.client.SubagentDriver
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.concurrent.Promise

final class SubagentKeeper(
  persistence: StatePersistence[AgentState],
  jobLauncherConf: JobLauncherConf,
  agentConf: AgentConfiguration,
  actorSystem: ActorSystem)
{
  private val mutableFixedPriority = new FixedPriority
  private val stdoutCommitDelay = agentConf.config
    .getDuration("js7.order.stdout-stderr.commit-delay").toFiniteDuration
  private val started = SetOnce[Started]
  private val state = AsyncVariable[State](State.empty)
  private val orderToWaitForDriver = AsyncMap.empty[OrderId, Promise[Unit]]
  private val orderToDriver = AsyncMap.empty[OrderId, SubagentDriver]

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
            val driver = newLocalSubagentDriver(LegacySubagentId, started)
            driver.start *>
              Task(state.insert(driver, None).orThrow)
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
        state.clear -> state.idToDriver.values))
      .flatMap(drivers => Task
        .parSequenceUnordered(drivers.map(_.stop(Some(SIGKILL)))))
      .void

  def processOrder(
    order: Order[Order.IsFreshOrReady],
    defaultArguments: Map[String, Expression],
    onEvents: (Seq[OrderCoreEvent], AgentState) => Unit)
  : Task[Checked[Unit]] =
    Task.defer {
      val promise = Promise[Unit]()
      // TODO Long lock on orderId !
      orderToWaitForDriver.update(order.id, _ => Task.pure(promise)) *>
        Task
          .race(Task.fromFuture(promise.future), selectSubagentDriver)
          .guarantee(orderToWaitForDriver.remove(order.id).void)
          .map(_.toOption)
          .flatMap {
            case None => Task.pure(Left(Problem(
              "Order has been canceled while selecting a Subagent")))
              Task.pure(Checked.unit) // ???

            case Some(driver) =>
              // OrderStarted automatically with first OrderProcessingStarted
              val events = order.isState[Order.Fresh].thenList(OrderStarted) :::
                OrderProcessingStarted(driver.subagentId) :: Nil
              persistence.persistKeyedEvents(events.map(order.id <-: _))
                .flatMapT { case (_, s) =>
                  onEvents(events, s)
                  orderToDriver.update(order.id, _ => Task.pure(driver)) *>
                    driver
                      .processOrder(
                        s.idToOrder(order.id).checkedState[Order.Processing].orThrow,
                        defaultArguments)
                      .guarantee(orderToDriver.remove(order.id).void)
                      .map(Right(_))
                }
                .flatMapT { outcome =>
                  val event = OrderProcessed(outcome)
                  persistence.persistKeyedEvent(order.id <-: event)
                    .map(_.map { case (_, s) => onEvents(event :: Nil, s) })
                    .rightAs(())
                }
          }
      // TODO Fehler in OrderProcessed(Outcome.Disrupted/Failed) fangen
    }

  private def selectSubagentDriver: Task[SubagentDriver] = {
    Observable
      .repeatEvalF(Coeval {
        state.get.selectNext(mutableFixedPriority)
      })
      .delayOnNextBySelector {
        case None => Observable.unit.delayExecution(1.s/*TODO Do not poll*/)
        case Some(_) => Observable.empty
      }
      .flatMap(Observable.fromIterable(_))
      .headL
  }

  def killProcess(orderId: OrderId, signal: ProcessSignal): Task[Unit] =
    Task.defer {
      // TODO Race condition?
      orderToWaitForDriver
        .get(orderId)
        .fold(Task.unit)(promise => Task(promise.success(())))
        .*>(orderToDriver
          .get(orderId)
          .match_ {
            case None => Task(logger.warn(s"killProcess($orderId): Unknown OrderId"))
            case Some(driver) => driver.killProcess(orderId, signal)
          })
    }

  def remove(subagentId: SubagentId): Task[Unit] =
    state
      .updateWithResult(state => Task(
        state.remove(subagentId) ->
          state.idToDriver.get(subagentId)))
      .flatMap(_.fold(Task.unit)(_.stop(Some(SIGKILL))))
      .void

  def proceed(subagentRefState: SubagentRefState): Task[Unit] = {
    val subagentRef = subagentRefState.subagentRef
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

                case Some(existing: RemoteSubagentDriver) =>
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
                    for (updated <- state.insert(driver, subagentRef.priority)) yield
                      updated -> Some(driver)
                  }

                case None =>
                  Task.deferAction(implicit s => Task {
                    val subagentDriver = newSubagentDriver(subagentRef, started)
                    for (s <- state.insert(subagentDriver, subagentRef.priority)) yield
                      s -> Some(subagentDriver)
                  })
              })
          .map(o => o: Checked[Option[SubagentDriver]])
          .flatMapT {
            case None => Task.pure(Checked.unit)
            case Some(driver) => driver.start.map(Right(_))
          }
          .void)
  }

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

  private case class Started(
    agentPath: AgentPath,
    localSubagentId: Option[SubagentId],
    controllerId: ControllerId)

  private final case class State(
    idToEntry: Map[SubagentId, Entry],
    prioritized: Prioritized[SubagentId, Entry])
  {
    def idToDriver = idToEntry.view.mapValues(_.driver)

    def insert(driver: SubagentDriver, priority: Option[Int]): Checked[State] = {
      val entry = Entry(driver, priority)
      for {
        idToE <- idToEntry.insert(driver.subagentId -> entry)
        prio <- prioritized.insert(entry)
      } yield copy(
        idToEntry = idToE,
        prioritized = prio)
    }

    def remove(subagentId: SubagentId): State =
      copy(
        idToEntry = idToEntry.removed(subagentId),
        prioritized = prioritized.remove(subagentId))

    def clear: State =
      copy(
        idToEntry = Map.empty,
        prioritized = prioritized.clear)

    def selectNext(fixedPriority: FixedPriority): Option[SubagentDriver] =
      prioritized
        .selectNext(fixedPriority, _.driver.isAlive)
        .map(_.driver)
  }
  private object State {
    val empty = State(
      Map.empty,
      Prioritized.empty[SubagentId, Entry](
        vToK = _.driver.subagentId,
        toPriority = _.priority.getOrElse(Int.MinValue)))
  }

  private final case class Entry(driver: SubagentDriver, priority: Option[Int])
}
