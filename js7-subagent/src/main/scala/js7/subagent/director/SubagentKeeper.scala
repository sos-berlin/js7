package js7.subagent.director

import akka.actor.ActorSystem
import cats.effect.Resource
import cats.implicits.catsSyntaxParallelUnorderedTraverse
import cats.instances.option._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.traverse._
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.CorrelIdBinder.{bindCorrelId, currentCorrelId}
import js7.base.log.Logger
import js7.base.log.Logger.syntax._
import js7.base.monixutils.MonixBase.syntax._
import js7.base.monixutils.{AsyncMap, AsyncVariable}
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.{DelayIterator, DelayIterators}
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.checkedCast
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{LockKeeper, SetOnce}
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.delegate.DelegateCouplingState.Coupled
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.item.BasicItemEvent.ItemDetached
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.Problems.ProcessLostDueSubagentUriChangeProblem
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentResetStarted}
import js7.data.subagent.{SubagentDirectorState, SubagentId, SubagentItem, SubagentItemState, SubagentSelection, SubagentSelectionId}
import js7.journal.state.StatePersistence
import js7.launcher.configuration.JobLauncherConf
import js7.subagent.configuration.DirectorConf
import js7.subagent.director.SubagentKeeper._
import js7.subagent.{LocalSubagentDriver, SubagentDriver}
import monix.eval.{Coeval, Task}
import monix.reactive.Observable
import scala.concurrent.Promise

final class SubagentKeeper[S <: SubagentDirectorState[S]](
  agentPath: AgentPath,
  persistence: StatePersistence[S],
  jobLauncherConf: JobLauncherConf,
  directorConf: DirectorConf,
  actorSystem: ActorSystem)
{
  private var reconnectDelayer: DelayIterator = null
  private val legacyLocalSubagentId = SubagentId.legacyLocalFromAgentPath(agentPath) // COMPATIBLE with v2.2
  private val driverConf = SubagentDriver.Conf.fromConfig(directorConf.config,
    commitDelay = directorConf.journalConf.delay)
  /** defaultPrioritized is used when no SubagentSelectionId is given. */
  private val defaultPrioritized = Prioritized.empty[SubagentId](
    toPriority = _ => 0/*same priority for each entry, round-robin*/)
  private val initialized = SetOnce[Initialized]
  private val stateVar = AsyncVariable[State](State(Map.empty, Map(
    /*local Subagent*/None -> defaultPrioritized)))
  private val orderToWaitForSubagent = AsyncMap.empty[OrderId, Promise[Unit]]
  private val orderToSubagent = AsyncMap.empty[OrderId, SubagentDriver]
  private val subagentItemLockKeeper = new LockKeeper[SubagentId]

  def initialize(localSubagentId: Option[SubagentId], controllerId: ControllerId): Task[Unit] =
    Task.deferAction { scheduler =>
      reconnectDelayer = DelayIterators
        .fromConfig(directorConf.config, "js7.subagent-driver.reconnect-delays")(scheduler)
        .orThrow

      val initialized = Initialized(agentPath, localSubagentId, controllerId)
      this.initialized := initialized
      if (localSubagentId.isDefined)
        Task.unit
      else // COMPATIBLE with v2.2 which does not know Subagents
        stateVar
          .update(state => Task(state
            .insertSubagentDriver(newLocalSubagentDriver(legacyLocalSubagentId, initialized))
            .orThrow))
          .void
    }

  def start: Task[Unit] =
    logger.debugTask(Task.defer {
      initialized.orThrow // Must be initialized!

      continueDetaching *>
        stateVar.get.idToDriver.values
          .toVector
          .parUnorderedTraverse(_.start)
          .map(_.combineAll)
    })

  def stop: Task[Unit] =
    stateVar
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
    onEvents: Seq[OrderCoreEvent] => Unit)
  : Task[Checked[Unit]] =
    selectSubagentDriverCancelable(order.id).flatMapT {
      case None =>
        logger.debug(s"⚠️ ${order.id} has been canceled while selecting a Subagent")
        Task.right(())

      case Some(driver) =>
        processOrderAndForwardEvents(order, onEvents, driver)
    }

  private def processOrderAndForwardEvents(
    order: Order[Order.IsFreshOrReady],
    onEvents: Seq[OrderCoreEvent] => Unit,
    subagentDriver: SubagentDriver)
  : Task[Checked[Unit]] = {
    // TODO Race with CancelOrders ?
    val startedEvents = order.isState[Order.Fresh].thenList(OrderStarted) :::
      OrderProcessingStarted(subagentDriver.subagentId) :: Nil
    persist(order.id, startedEvents, onEvents)
      .map(_.map { case (_, s) => s
        .idToOrder
        .checked(order.id)
        .flatMap(_.checkedState[Order.Processing])
        .orThrow
      })
      .flatMapT(order =>
        forProcessingOrder(order.id, subagentDriver, onEvents)(
          subagentDriver.processOrder(order)))
      .onErrorHandle { t =>
        logger.error(s"processOrder ${order.id} => ${t.toStringWithCauses}", t.nullIfNoStackTrace)
        Left(Problem.fromThrowable(t))
      }
  }

  def continueProcessingOrder(
    order: Order[Order.Processing],
    onEvents: Seq[OrderCoreEvent] => Unit)
  : Task[Checked[Unit]] =
    logger.traceTask("continueProcessingOrder", order.id)(Task.defer {
      val subagentId = order.state.subagentId getOrElse legacyLocalSubagentId
      stateVar.get.idToDriver.get(subagentId)
        .match_ {
          case None =>
            val event = OrderProcessed(Outcome.Disrupted(Problem.pure(
              s"Missing $subagentId SubagentItem to continue processing of ${order.id}")))
            persist(order.id, event :: Nil, onEvents)
              .map(_.map { case (_, s) => event -> s })

          case Some(subagentDriver) =>
            forProcessingOrder(order.id, subagentDriver, onEvents)(
              subagentDriver.continueProcessingOrder(order)
            ).materializeIntoChecked
              .map(_.onProblemHandle(problem =>
                logger.error(s"continueProcessingOrder ${order.id} => $problem")))
              .startAndForget // ???
              .as(Checked.unit)
        }
    })

  private def persist(
    orderId: OrderId,
    events: Seq[OrderCoreEvent],
    onEvents: Seq[OrderCoreEvent] => Unit)
  : Task[Checked[(Seq[Stamped[KeyedEvent[OrderCoreEvent]]], S)]] =
    persistence
      .persistKeyedEvents(events.map(orderId <-: _))
      .map(_.map { o =>
        onEvents(events)
        o
      })

  private def forProcessingOrder(
    orderId: OrderId,
    subagentDriver: SubagentDriver,
    onEvents: Seq[OrderCoreEvent] => Unit)
    (body: Task[Checked[OrderProcessed]])
  : Task[Checked[Unit]] =
    Resource
      .make(
        acquire = orderToSubagent.put(orderId, subagentDriver).void)(
        release = _ => orderToSubagent.remove(orderId).void)
      .use(_ =>
        body
          .map(_.map(orderProcessed =>
            // OrderProcessed event has been persisted by SubagentDriver
            onEvents(orderProcessed :: Nil))))

  private def selectSubagentDriverCancelable(orderId: OrderId)
  : Task[Checked[Option[SubagentDriver]]] =
    orderToSubagentSelectionId(orderId)
      .flatMapT(maybeSelectionId =>
        cancelableWhileWaitingForSubagent(orderId)
          .use(canceledPromise =>
            Task.race(
              Task.fromFuture(bindCorrelId(currentCorrelId)(canceledPromise.future)),
              selectSubagentDriver(maybeSelectionId)))
          .map(_.toOption)
          .map(Right(_)))

  private def orderToSubagentSelectionId(orderId: OrderId)
  : Task[Checked[Option[SubagentSelectionId]]] =
    for (agentState <- persistence.state) yield
      for {
        order <- agentState.idToOrder.checked(orderId)
        job <- agentState.workflowJob(order.workflowPosition)
      } yield job.subagentSelectionId

  /** While waiting for a Subagent, the Order is cancelable. */
  private def cancelableWhileWaitingForSubagent(orderId: OrderId): Resource[Task, Promise[Unit]] =
    Resource
      .eval(Task(Promise[Unit]()))
      .flatMap(canceledPromise =>
        Resource.make(
          acquire = orderToWaitForSubagent.put(orderId, canceledPromise))(
          release = _ => orderToWaitForSubagent.remove(orderId).void))

  private def selectSubagentDriver(maybeSelectionId: Option[SubagentSelectionId])
  : Task[SubagentDriver] =
    Observable
      .repeatEvalF(Coeval { stateVar.get.selectNext(maybeSelectionId) }
        .flatTap(o => Coeval(
          logger.trace(s"selectSubagentDriver($maybeSelectionId) => $o ${stateVar.get}"))))
      .delayOnNextBySelector {
        // TODO Do not poll (for each Order)
        case None => Observable.unit.delayExecution(reconnectDelayer.next())
        case Some(_) =>
          reconnectDelayer.reset()
          Observable.empty
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

  def startResetSubagent(subagentId: SubagentId, force: Boolean): Task[Checked[Unit]] =
    stateVar.value
      .flatMap(s => Task(s.idToDriver.checked(subagentId)))
      .flatMapT {
        case driver: RemoteSubagentDriver[S] @unchecked =>
          persistence.persistKeyedEvent(subagentId <-: SubagentResetStarted(force))
            .flatMapT(_ =>
              driver.reset(force)
                .onErrorHandle(t =>
                  logger.error(s"$subagentId reset => $t", t.nullIfNoStackTrace))
                .startAndForget
                .map(Right(_)))
        case _ =>
          Task.pure(Problem.pure(s"$subagentId as the Agent Director cannot be reset"))
      }

  def startRemoveSubagent(subagentId: SubagentId): Task[Unit] =
    removeSubagent(subagentId)
      .onErrorHandle[Unit](t => Task(logger.error(s"removeSubagent($subagentId) => $t")))
      .startAndForget

  private def removeSubagent(subagentId: SubagentId): Task[Unit] =
    logger.debugTask("removeSubagent", subagentId)(
      stateVar.value
        .flatMap(_.idToDriver
          .get(subagentId)
          .fold(Task.unit)(subagentDriver =>
            subagentDriver.tryShutdown
              .*>(stateVar.update(state => Task(
                state.removeSubagent(subagentId))))
              .*>(subagentDriver.stop(signal = None)))))
              .*>(persistence
                .persistKeyedEvent(ItemDetached(subagentId, agentPath))
                .orThrow
                .void)

  def recoverSubagents(subagentItemStates: Seq[SubagentItemState]): Task[Checked[Unit]] =
    subagentItemStates
      .traverse(s => addOrChange(s)
        .map(_.map(_.map(_ => s))))
      .map(_.combineProblems)
      .map(_.map(_.flatten))

  private def continueDetaching: Task[Checked[Unit]] =
    persistence.state.flatMap(_
      .idToSubagentItemState.values
      .view
      .collect { case o if o.isDetaching => o.subagentId }
      .toVector
      .traverse(startRemoveSubagent)
      .map(_.combineAll)
      .map(Right(_)))

  def recoverSubagentSelections(subagentSelections: Seq[SubagentSelection]): Task[Checked[Unit]] =
    logger.debugTask(
      subagentSelections
        .traverse(addOrReplaceSubagentSelection)
        .map(_.combineAll))

  // TODO Kann SubagentItem gelöscht werden während proceed hängt wegen unerreichbaren Subagenten?
  def proceedWithSubagent(subagentItemState: SubagentItemState): Task[Checked[Unit]] =
    logger.traceTask("proceedWithSubagent", subagentItemState.pathRev)(
      addOrChange(subagentItemState)
        .flatMapT(_
          .fold(Task.unit)(_
            .start
            .onErrorHandle(t => logger.error( // TODO Emit event ?
              s"proceedWithSubagent(${subagentItemState.pathRev}) => ${t.toStringWithCauses}"))
            /*.startAndForget*/)
        .map(Right(_))))

  // May return a new, non-started SubagentDriver
  private def addOrChange(subagentItemState: SubagentItemState)
  : Task[Checked[Option[SubagentDriver]]] =
    logger.debugTask("addOrChange", subagentItemState.pathRev) {
      val subagentItem = subagentItemState.subagentItem
      initialized.task
        .logWhenItTakesLonger("SubagentKeeper.initialized?")
        .flatMap(initialized =>
          stateVar.get.idToDriver.get(subagentItem.id) match {
            case Some(_: LocalSubagentDriver[_]) =>
              stateVar
                .updateChecked(state => Task(
                  state.disable(subagentItem.id, subagentItem.disabled)))
                .rightAs(None)

            case _ =>
              // Don't use the matched RemoteAgentDriver. We update state with an atomic operation.
              stateVar.updateCheckedWithResult(state =>
                Task(state.idToDriver.get(subagentItem.id) match {
                  case None =>
                    val subagentDriver = newSubagentDriver(subagentItem, initialized)
                    state.insertSubagentDriver(subagentDriver, subagentItem)
                      .flatMap(_.disable(subagentItem.id, subagentItem.disabled))
                      .map(_ -> Some(None -> subagentDriver))

                  case Some(existing) =>
                    checkedCast[RemoteSubagentDriver[S]](existing)
                      .flatMap(existing =>
                        if (subagentItem.uri == existing.subagentItem.uri)
                          Right(state -> None)
                        else {
                          // Subagent moved
                          val driver = newRemoteSubagentDriver(subagentItem, initialized)
                          state.replaceSubagentDriver(driver, subagentItem)
                            .map(_ -> Some(Some(existing) -> driver))
                          // Continue after locking updateCheckedWithResult
                        })
                      .flatMap { case (state, result) =>
                        state.disable(subagentItem.id, subagentItem.disabled)
                          .map(_ -> result)
                      }
                }))
          })
        .flatMapT {
          case Some((Some(oldDriver), newDriver: RemoteSubagentDriver[S] @unchecked)) =>
            assert(oldDriver.subagentId == newDriver.subagentId)
            val name = "addOrChange " + oldDriver.subagentItem.pathRev
            oldDriver
              // FIXME Kill the processes ?
              .stopDispatcherAndEmitProcessLostEvents(ProcessLostDueSubagentUriChangeProblem, None)
              .*>(oldDriver.stop)  // Maybe try to send Shutdown command ???
              .*>(subagentItemLockKeeper
                .lock(oldDriver.subagentId)(
                  newDriver.startMovedSubagent(oldDriver))
                .logWhenItTakesLonger(name)
                  .onErrorHandle(t => logger.error(
                    s"addOrChange $name => ${t.toStringWithCauses}", t.nullIfNoStackTrace))
                .startAndForget
                .as(Right(None)))

          case Some((None, newDriver: LocalSubagentDriver[_])) =>
            coupleLocalSubagent
              .as(Right(Some(newDriver)))

          case maybeNewDriver => Task.right(maybeNewDriver.map(_._2))
        }
    }

  private def coupleLocalSubagent: Task[Unit] =
    Task.defer {
      initialized.orThrow.localSubagentId.traverse(localSubagentId =>
        persistence
          .persist(agentState => Right(agentState
            .idToSubagentItemState.get(localSubagentId)
            .exists(_.couplingState != Coupled)
            .thenList(localSubagentId <-: SubagentCoupled)))
          .orThrow)
        .void
    }

  private def newSubagentDriver(subagentItem: SubagentItem, initialized: Initialized) =
    if (initialized.localSubagentId contains subagentItem.id)
      newLocalSubagentDriver(subagentItem.id, initialized)
    else
      newRemoteSubagentDriver(subagentItem, initialized)

  private def newLocalSubagentDriver(subagentId: SubagentId, initialized: Initialized) =
    new LocalSubagentDriver(
      subagentId,
      persistence,
      initialized.agentPath,
      initialized.controllerId,
      jobLauncherConf,
      driverConf,
      directorConf.subagentConf)

  private def newRemoteSubagentDriver(subagentItem: SubagentItem, initialized: Initialized) =
    new RemoteSubagentDriver(
      subagentItem,
      directorConf.httpsConfig,
      persistence,
      initialized.controllerId,
      driverConf,
      directorConf.subagentConf,
      directorConf.recouplingStreamReaderConf,
      actorSystem)

  def addOrReplaceSubagentSelection(selection: SubagentSelection): Task[Checked[Unit]] =
    stateVar
      .updateChecked(state => Task(state.insertOrReplaceSelection(selection)))
      .rightAs(())

  def removeSubagentSelection(subagentSelectionId: SubagentSelectionId): Task[Unit] =
    stateVar
      .update(state => Task(state.removeSelection(subagentSelectionId)))
      .void

  override def toString = s"SubagentKeeper(${orderToSubagent.size} processing orders)"
}

object SubagentKeeper
{
  private val logger = Logger[this.type]
  private val DefaultPriority = 0

  private case class Initialized(
    agentPath: AgentPath,
    localSubagentId: Option[SubagentId],
    controllerId: ControllerId)

  private final case class State(
    subagentToEntry: Map[SubagentId, Entry],
    selectionToPrioritized: Map[Option[SubagentSelectionId], Prioritized[SubagentId]])
  {
    def idToDriver = subagentToEntry.view.mapValues(_.driver)

    def insertSubagentDriver(driver: SubagentDriver, subagentItem: SubagentItem): Checked[State] =
      insertSubagentDriver(driver, subagentItem.disabled)

    def insertSubagentDriver(driver: SubagentDriver, disabled: Boolean = false): Checked[State] =
      for {
        idToE <- subagentToEntry.insert(driver.subagentId -> Entry(driver, disabled))
        selectionToPrioritized <-
          if (disabled)
            Right(selectionToPrioritized)
          else
            // Add SubagentId to default SubagentSelection
            for (o <- selectionToPrioritized(None).add(driver.subagentId)) yield
              selectionToPrioritized.updated(None, o)
      } yield copy(
        subagentToEntry = idToE,
        selectionToPrioritized = selectionToPrioritized)

    def replaceSubagentDriver(driver: SubagentDriver, subagentItem: SubagentItem): Checked[State] =
      if (!subagentToEntry.contains(driver.subagentId))
        Left(Problem(s"Replacing unknown ${driver.subagentId} SubagentDriver"))
      else
        Right(copy(
          subagentToEntry = subagentToEntry.updated(
            driver.subagentId,
            Entry(driver, subagentItem.disabled))))

    def removeSubagent(subagentId: SubagentId): State =
      copy(
        subagentToEntry = subagentToEntry.removed(subagentId),
        // Remove SubagentId from default SubagentSelection
        selectionToPrioritized =
          selectionToPrioritized.updated(None, selectionToPrioritized(None).remove(subagentId)))

    def insertOrReplaceSelection(selection: SubagentSelection): Checked[State] =
      Right(copy(
        selectionToPrioritized = selectionToPrioritized.updated(
          Some(selection.id),
          Prioritized[SubagentId](
            selection.subagentToPriority.keys,
            id => selection.subagentToPriority.getOrElse(id, {
              logger.error(s"${selection.id} uses unknown $id. Assuming priority=$DefaultPriority")
              DefaultPriority
            })))))

    def removeSelection(selectionId: SubagentSelectionId): State =
      copy(selectionToPrioritized = selectionToPrioritized - Some(selectionId))

    def clear: State =
      copy(
        subagentToEntry = Map.empty,
        selectionToPrioritized = Map.empty)

    def disable(id: SubagentId, disabled: Boolean): Checked[State] =
      subagentToEntry
        .get(id)
        .filter(_.disabled != disabled)
        .fold(Checked(this)) { entry =>
          val checkedSelToPrio =
            if (disabled)
              Right(selectionToPrioritized.view.mapValues(_.remove(id)).toMap)
            else
              selectionToPrioritized.toVector
                .traverse { case (k, v) => v.add(id).map(k -> _) }
                .map(_.toMap)
          for (selToPrio <- checkedSelToPrio) yield
            copy(
              subagentToEntry = subagentToEntry.updated(id, entry.copy(disabled = disabled)),
              selectionToPrioritized = selToPrio)
        }

    def selectNext(selectionId: Option[SubagentSelectionId]): Option[SubagentDriver] =
      selectionToPrioritized.get(selectionId) // May be non-existent when stopping
        .flatMap(_
          .selectNext(subagentId => subagentToEntry.get(subagentId).fold(false)(_.driver.isCoupled)))
        .flatMap(subagentId => subagentToEntry.get(subagentId).map(_.driver))
  }

  private final case class Entry(
    driver: SubagentDriver,
    disabled: Boolean = false)

  private final case class SelectionEntry(
    subagentSelectionId: SubagentSelectionId,
    fixedPriority: FixedPriority/*mutable*/)
}
