package js7.agent.subagent

import akka.actor.ActorSystem
import cats.effect.Resource
import cats.syntax.foldable._
import cats.syntax.parallel._
import cats.syntax.traverse._
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.subagent.SubagentKeeper._
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.log.Logger.syntax._
import js7.base.monixutils.MonixBase.syntax.{RichCheckedTask, RichMonixTask}
import js7.base.monixutils.{AsyncMap, AsyncVariable}
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SetOnce
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.order.OrderEvent.{LegacySubagentId, OrderCoreEvent, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.{SubagentId, SubagentRef, SubagentRefState}
import js7.data.value.expression.Expression
import js7.journal.state.StatePersistence
import js7.launcher.configuration.JobLauncherConf
import js7.subagent.LocalSubagentDriver
import js7.subagent.client.SubagentDriver
import monix.eval.{Coeval, Task}
import monix.reactive.Observable
import scala.concurrent.Promise

final class SubagentKeeper(
  persistence: StatePersistence[AgentState],
  jobLauncherConf: JobLauncherConf,
  agentConf: AgentConfiguration,
  actorSystem: ActorSystem)
{
  private val driverConf = SubagentDriver.Conf.fromConfig(agentConf.config)  // TODO
  private val mutableFixedPriority = new FixedPriority
  private val initialized = SetOnce[Initialized]
  private val state = AsyncVariable[State](State.empty)
  private val orderToWaitForSubagent = AsyncMap.empty[OrderId, Promise[Unit]]
  private val orderToSubagent = AsyncMap.empty[OrderId, SubagentDriver]

  def initialize(agentPath: AgentPath, localSubagentId: Option[SubagentId], controllerId: ControllerId)
  : Task[Unit] =
    Task.defer {
      val initialized = Initialized(agentPath, localSubagentId, controllerId)
      this.initialized := initialized
      if (localSubagentId.isDefined)
        Task.unit
      else  // COMPATIBLE with v2.1 AgentRef
        state
          .update(state => Task(state
            .insert(
              driver = newLocalSubagentDriver(LegacySubagentId, initialized),
              priority = None)
            .orThrow))
          .void
    }

  def start: Task[Unit] =
    logger.debugTask(
      Task(initialized.orThrow) *>
        state.get.idToDriver.values
          .toVector
          .parUnorderedTraverse(_.start)
          .map(_.combineAll))

  def stop: Task[Unit] =
    state
      .updateWithResult(state => Task(
        state.clear -> state.idToDriver.values))
      .flatMap(drivers =>
        drivers.toVector
          .parUnorderedTraverse(_.stop(Some(SIGKILL)))
          .map(_.combineAll))

  def orderIsLocal(orderId: OrderId): Boolean =
    orderToSubagent.toMap.get(orderId)
      .exists(_.isInstanceOf[LocalSubagentDriver[_]])

  def processOrder(
    order: Order[Order.IsFreshOrReady],
    defaultArguments: Map[String, Expression],
    onEvents: Seq[OrderCoreEvent] => Unit)
  : Task[Checked[Unit]] =
    selectSubagentDriverCancelable(order.id).flatMap {
      case None =>
        logger.debug(s"${order.id} has been canceled while selecting a Subagent")
        Task.right(())

      case Some(driver) =>
        processOrderAndForwardEvents(order, defaultArguments, onEvents, driver)
    }

  private def processOrderAndForwardEvents(
    order: Order[Order.IsFreshOrReady],
    defaultArguments: Map[String, Expression],
    onEvents: Seq[OrderCoreEvent] => Unit,
    subagentDriver: SubagentDriver)
  : Task[Checked[Unit]] = {
    // TODO Race with CancelOrders ?
    val startedEvents = order.isState[Order.Fresh].thenList(OrderStarted) :::
      OrderProcessingStarted(subagentDriver.subagentId) :: Nil
    persist(order.id, startedEvents, onEvents)
      .map(_.map { case (_, s) =>
        s.idToOrder
          .checked(order.id)
          .flatMap(_.checkedState[Order.Processing])
          .orThrow
      })
    .flatMapT(order =>
      forProcessingOrder(order.id, subagentDriver, onEvents)(
        subagentDriver.processOrder(order, defaultArguments)))
    .onErrorHandle { t =>
      logger.error(s"processOrder ${order.id} => ${t.toStringWithCauses}", t.nullIfNoStackTrace)
      Left(Problem.fromThrowable(t))
    }
  }

  private def persist(
    orderId: OrderId,
    events: Seq[OrderCoreEvent],
    onEvents: Seq[OrderCoreEvent] => Unit)
  : Task[Checked[(Seq[Stamped[KeyedEvent[OrderCoreEvent]]], AgentState)]] =
    persistence
      .persistKeyedEvents(events.map(orderId <-: _))
      .map(_.map { o =>
        onEvents(events)
        o
      })

  def continueProcessingOrder(
    order: Order[Order.Processing],
    onEvents: Seq[OrderCoreEvent] => Unit)
  : Task[Checked[Unit]] =
    logger.traceTask("continueProcessingOrder", order.id)(Task.defer {
      val subagentId = order.state.subagentId
      state.get.idToDriver.get(subagentId)
        .match_ {
          case None =>
            val event = OrderProcessed(Outcome.Disrupted(Problem.pure(
              s"Missing $subagentId SubagentDriver for processing ${ order.id }")))
            persist(order.id, event :: Nil, onEvents)
              .map(_.map { case (_, s) => event -> s })

          case Some(subagentDriver) =>
            forProcessingOrder(order.id, subagentDriver, onEvents)(
              subagentDriver.continueProcessingOrder(order)
            ).materializeIntoChecked
        }
    })

  private def forProcessingOrder(orderId: OrderId, subagentDriver: SubagentDriver, onEvents: Seq[OrderCoreEvent] => Unit)
    (body: Task[Checked[OrderProcessed]])
  : Task[Checked[Unit]] =
    Resource.make(
      acquire = orderToSubagent.put(orderId, subagentDriver).void)(
      release = _ => orderToSubagent.remove(orderId).void
    ).use(_ =>
      body.map(_.map(processed => onEvents(processed :: Nil)))
    )

  private def selectSubagentDriverCancelable(orderId: OrderId): Task[Option[SubagentDriver]] =
    cancelableWhileWaitingForSubagent(orderId)
      .use(canceledPromise =>
        Task.race(
          Task.fromFuture(canceledPromise.future),
          selectSubagentDriver))
      .map(_.toOption)

  /** While waiting for a Subagent, the Order is cancelable. */
  private def cancelableWhileWaitingForSubagent(orderId: OrderId): Resource[Task, Promise[Unit]] =
    Resource
      .eval(Task(Promise[Unit]()))
      .flatMap(canceledPromise =>
        Resource.make(
          acquire = orderToWaitForSubagent.put(orderId, canceledPromise))(
          release = _ => orderToWaitForSubagent.remove(orderId).void))

  private def selectSubagentDriver: Task[SubagentDriver] =
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

  def killProcess(orderId: OrderId, signal: ProcessSignal): Task[Unit] =
    Task.defer {
      // TODO Race condition?
      orderToWaitForSubagent
        .get(orderId)
        .fold(Task.unit)(promise => Task(promise.success(())))
        .*>(orderToSubagent
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

  def recoverSubagents(subagentRefStates: Seq[SubagentRefState]): Task[Checked[Unit]] =
    subagentRefStates
      .traverse(addOrChange)
      .map(_
        .collectFirst { case Left(problem) => Left(problem) }
        .getOrElse(Checked.unit))

  // TODO Kann SubagentRef gelöscht werden während proceed hängt wegen unerreichbarem Subagenten?
  def proceedWithSubagent(subagentRefState: SubagentRefState): Task[Checked[Unit]] =
    logger.traceTask("proceedWithSubagent", subagentRefState.subagentId)(
      addOrChange(subagentRefState)
        .flatMapT(_
          .fold(Task.unit)(_
            .start
            .onErrorHandle(t => logger.error( // TODO Emit event ?
              s"proceedWithSubagent(${subagentRefState.subagentId}) => ${t.toStringWithCauses}"))
            .startAndForget)
          .map(Right(_))))

  // Returns a SubagentDriver if created
  private def addOrChange(subagentRefState: SubagentRefState)
  : Task[Checked[Option[SubagentDriver]]] = {
    val subagentRef = subagentRefState.subagentRef
    logger.traceTask("addOrChange", subagentRefState.subagentId)(
      initialized.task
        .logWhenItTakesLonger("SubagentKeeper.initialized?")
        .flatMap(initialized =>
          state
            .updateCheckedWithResult(state =>
              state.idToDriver
                .get(subagentRef.id)
                .match_ {
                  case Some(_: LocalSubagentDriver[_]) =>
                    Task.pure(
                      for (updated <- state.disable(subagentRef.id, subagentRef.disabled)) yield
                        updated -> None)

                  case Some(existing: RemoteSubagentDriver) =>
                    (if (subagentRef.uri == existing.subagentRef.uri)
                      Task.unit
                    else
                      existing
                        .stop(Some(SIGKILL))
                        .onErrorHandle(t => logger.error("After change of URI of " +
                          existing.subagentId + " " + existing.subagentRef.uri + ": " +
                          t.toStringWithCauses))
                        .startAndForget // Do not block the `state` here, so startAndForget ???
                    ).map { _ =>
                      // TODO subagentRef.disable!
                      // Replace and forget the existing driver
                      val driver = newRemoteSubagentDriver(subagentRef, initialized)
                      for (updated <- state.insert(driver, subagentRef)) yield
                        updated -> Some(driver)
                    }

                  case None =>
                    Task {
                      val subagentDriver = newSubagentDriver(subagentRef, initialized)
                      for (s <- state.insert(subagentDriver, subagentRef)) yield
                        s -> Some(subagentDriver)
                    }
                })))
  }

  private def newSubagentDriver(subagentRef: SubagentRef, initialized: Initialized) =
    if (initialized.localSubagentId contains subagentRef.id)
      newLocalSubagentDriver(subagentRef.id, initialized)
    else
      newRemoteSubagentDriver(subagentRef, initialized)

  private def newLocalSubagentDriver(subagentId: SubagentId, initialized: Initialized) =
    new LocalSubagentDriver(
      subagentId,
      persistence,
      initialized.agentPath,
      initialized.controllerId,
      jobLauncherConf,
      driverConf,
      valueDirectory = agentConf.subagentConf.valueDirectory)

  private def newRemoteSubagentDriver(subagentRef: SubagentRef, initialized: Initialized) =
    new RemoteSubagentDriver(
      subagentRef,
      for (subagentId <- initialized.localSubagentId) yield
        UserAndPassword(
          UserId.checked(subagentId.string).orThrow,
          SecretString.empty),
      agentConf.httpsConfig,
      persistence,
      initialized.controllerId,
      driverConf,
      agentConf.recouplingStreamReaderConf,
      actorSystem)

  def isInLocalProcess(orderId: OrderId) =
    orderToSubagent.get(orderId).exists(_.isInstanceOf[LocalSubagentDriver[_]])
}

object SubagentKeeper
{
  private val logger = Logger[this.type]

  private case class Initialized(
    agentPath: AgentPath,
    localSubagentId: Option[SubagentId],
    controllerId: ControllerId)

  private final case class State(
    idToEntry: Map[SubagentId, Entry],
    prioritized: Prioritized[SubagentId, Entry])
  {
    def idToDriver = idToEntry.view.mapValues(_.driver)

    def insert(driver: SubagentDriver, subagentRef: SubagentRef): Checked[State] =
      insert(driver, subagentRef.priority, subagentRef.disabled)

    def insert(driver: SubagentDriver, priority: Option[Int], disabled: Boolean = false)
    : Checked[State] = {
      val entry = Entry(driver, priority, disabled)
      for {
        idToE <- idToEntry.insert(driver.subagentId -> entry)
        prio <- if (disabled) Right(prioritized) else prioritized.insert(entry)
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

    def disable(id: SubagentId, disabled: Boolean): Checked[State] =
      idToEntry
        .get(id)
        .filter(_.disabled != disabled)
        .fold(Checked(this)) { entry =>
          val checkedPrio =
            if (disabled) Right(prioritized.remove(id))
            else prioritized.insert(entry)
          for (prio <- checkedPrio) yield
            copy(
              idToEntry = idToEntry.updated(id, entry.copy(disabled = disabled)),
              prioritized = prio)
        }

    def selectNext(fixedPriority: FixedPriority): Option[SubagentDriver] =
      prioritized
        .selectNext(fixedPriority, _.driver.isHeartbeating)
        .map(_.driver)
  }
  private object State {
    val empty = State(
      Map.empty,
      Prioritized.empty[SubagentId, Entry](
        vToK = _.driver.subagentId,
        toPriority = _.priority.getOrElse(Int.MinValue)))
  }

  private final case class Entry(
    driver: SubagentDriver,
    priority: Option[Int],
    disabled: Boolean = false)
}
