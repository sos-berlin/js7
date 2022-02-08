package js7.agent.subagent

import akka.actor.ActorSystem
import cats.effect.Resource
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
import js7.base.utils.Collections.implicits.RichIterableOnce
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
    Task.deferAction { implicit scheduler =>
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
          .map(_.fold_))

  def stop: Task[Unit] =
    state
      .updateWithResult(state => Task(
        state.clear -> state.idToDriver.values))
      .flatMap(drivers =>
        drivers.toVector
          .parUnorderedTraverse(_.stop(Some(SIGKILL)))
          .map(_.fold_))

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
  : Task[Checked[Unit]] =
  {
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
      forProcessingOrder(order.id, subagentDriver)(
        subagentDriver.processOrder(order, defaultArguments)))
    .onErrorHandle { t =>
      logger.error(s"${order.id}: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
      Left(Problem.fromThrowable(t))
    }
    .map(Outcome.leftToDisrupted)
    .flatMap(outcome =>
      persist(order.id, OrderProcessed(outcome) :: Nil, onEvents)
        .rightAs(()))
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
  : Task[Checked[(OrderProcessed, AgentState)]] =
    continueProcessingOrder2(order)
      .flatMapT { outcome =>
        val event = OrderProcessed(outcome)
        persist(order.id, event :: Nil, onEvents)
          .map(_.map { case (_, s) => event -> s })
      }

  private def continueProcessingOrder2(order: Order[Order.Processing]): Task[Checked[Outcome]] =
    Task.defer {
      val subagentId = order.state.subagentId
      state.get.idToDriver.get(subagentId)
        .match_ {
          case None => Task.pure(Right(Outcome.Disrupted(Problem.pure(
            s"Missing $subagentId for processing ${order.id} after Agent Director restart"))))

          case Some(subagentDriver) =>
            forProcessingOrder(order.id, subagentDriver)(
              subagentDriver.continueProcessingOrder(order)
            ).materializeIntoChecked
        }
    }

  private def forProcessingOrder(
    orderId: OrderId,
    subagentDriver: SubagentDriver)(
    body: Task[Checked[Outcome]])
  : Task[Checked[Outcome]] =
    Resource.make(
      acquire = orderToSubagent.put(orderId, subagentDriver).void)(
      release = _ => orderToSubagent.remove(orderId).void
    ).use(_ => body)

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

  def proceed(subagentRefState: SubagentRefState): Task[Checked[Unit]] =
    addOrChange(subagentRefState)
      .flatMapT(_
        .start.map(Right(_)))

  def addOrChangeMultiple(subagentRefStates: Iterable[SubagentRefState]): Task[Checked[Unit]] =
    subagentRefStates
      .toVector
      .traverse(addOrChange)
      .map(_
        .collectFirst { case Left(problem) => Left(problem) }
        .getOrElse(Checked.unit))

  def addOrChange(subagentRefState: SubagentRefState): Task[Checked[SubagentDriver]] = {
    val subagentRef = subagentRefState.subagentRef
    initialized.task
      .logWhenItTakesLonger("SubagentKeeper.initialized?")
      .flatMap(initialized =>
        state
          .updateCheckedWithResult(state =>
            state.idToDriver
              .get(subagentRef.id)
              .match_ {
                case Some(existing: LocalSubagentDriver[_]) =>
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
                      .startAndForget // Do not block the `state` here, so startAndForget ???
                  ).map { _ =>
                    // Replace and forget the existing driver
                    val driver = newRemoteSubagentDriver(subagentRef, initialized)
                    for (updated <- state.insert(driver, subagentRef.priority)) yield
                      updated -> driver
                  }

                case None =>
                  Task.deferAction(implicit s => Task {
                    val subagentDriver = newSubagentDriver(subagentRef, initialized)
                    for (s <- state.insert(subagentDriver, subagentRef.priority)) yield
                      s -> subagentDriver
                  })
              }))
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

  private final case class Entry(driver: SubagentDriver, priority: Option[Int])
}
