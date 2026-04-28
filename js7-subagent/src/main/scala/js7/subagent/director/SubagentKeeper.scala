package js7.subagent.director

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, FiberIO, IO, Resource, ResourceIO}
import cats.instances.option.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.flatMap.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.typesafe.config.ConfigUtil
import fs2.Stream
import izumi.reflect.Tag
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.catsutils.CatsEffectExtensions.{catchIntoChecked, guaranteeExceptWhenRight, joinStd, orThrow, right, startAndForget}
import js7.base.catsutils.CatsExtensions.flatMapSome
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.generic.SecretString
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.headL
import js7.base.monixutils.{AsyncMap, AsyncVariable}
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.time.{DelayIterator, DelayIterators, Timestamp}
import js7.base.utils.Assertions.{assertIfStrict, assertThat}
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ConcurrentHashMap, LockKeeper, SetOnce}
import js7.core.command.CommandMeta
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.delegate.DelegateCouplingState.Coupled
import js7.data.event.{Event, EventCalc, KeyedEvent}
import js7.data.item.BasicItemEvent.ItemDetached
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.subagent.Problems.{ProcessLostDueSubagentDeletedProblem, ProcessLostDueSubagentUriChangeProblem}
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentResetStarted}
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentCommand, SubagentDirectorState, SubagentId, SubagentItem, SubagentItemState}
import js7.data.value.expression.Scope
import js7.data.value.{NumberValue, Value}
import js7.data.workflow.Workflow
import js7.journal.CommitOptions.Transaction
import js7.journal.Persisted.ifPersisted
import js7.journal.{Journal, Persisted}
import js7.subagent.Subagent
import js7.subagent.configuration.DirectorConf
import js7.subagent.director.SubagentKeeper.*
import org.apache.pekko.actor.ActorSystem
import org.jetbrains.annotations.TestOnly

final class SubagentKeeper[S <: SubagentDirectorState[S]] private(
  localSubagentId: SubagentId,
  localSubagent: Subagent,
  agentPath: AgentPath,
  controllerId: ControllerId,
  failedOverSubagentId: Option[SubagentId],
  journal: Journal[S], // TODO Let the only OrderMotor pipeline persist!
  directorConf: DirectorConf,
  actorSystem: ActorSystem)
  (implicit ioRuntime: IORuntime, sTag: Tag[S])
extends Service.StoppableByRequest:

  private val reconnectDelayer: DelayIterator = DelayIterators
    .fromConfig(directorConf.config, "js7.subagent-driver.reconnect-delays")(
      using ioRuntime.scheduler)
    .orThrow
  private lazy val legacyLocalSubagentId = SubagentId.legacyLocalFromAgentPath(agentPath) // COMPATIBLE with v2.2
  private val stateVar = AsyncVariable(DirectorState.initial(directorConf))
  private val orderToWaitForSubagent = ConcurrentHashMap[OrderId, Deferred[IO, Unit]]()
  private val orderToSubagent = AsyncMap.empty[OrderId, SubagentDriver]
  private val subagentItemLockKeeper = new LockKeeper[SubagentId]
  private val eventCallbackOnce = SetOnce[EventCallback]
  private val orderMotorCoupled = Deferred.unsafe[IO, Unit]
  private val serviceStarted = Deferred.unsafe[IO, Unit]
  @volatile private var started = false // Delays SubagentDriver#startObserving

  journal.untilStopped // TODO Terminate when journal dies

  def coupleWithOrderMotor(callback: EventCallback): IO[Unit] =
    logger.debugIO:
      IO.defer:
        eventCallbackOnce := callback
        orderMotorCoupled.complete(()).void *>
          serviceStarted.get.logWhenItTakesLonger("SubagentKeeper serviceStarted")

  protected def start =
    startService:
      orderMotorCoupled.get.logWhenItTakesLonger("SubagentKeeper orderMotorCoupled")
        .productR:
          start1
        .productR:
          serviceStarted.complete(())
        .productR:
          IO.defer:
            started = true
            IO.both(
              untilStopRequested *> stopMe,
              startObserving
            ).void

  private def start1 =
    journal.aggregate
      .flatMap: aggregate =>
        recoverSubagents(aggregate.idToSubagentItemState.values.toVector)
          .flatMapT: _ =>
            recoverSubagentBundles:
              aggregate.pathToUnsignedSimple(SubagentBundle).values.toVector
      .map(_.orThrow)
      .productR:
        continueDetachingSubagents

  private def stopMe: IO[Unit] =
    stateVar.updateWithResult: state =>
      IO.pure(state.clear -> state.subagentToEntry.values)
    .flatMap: entries =>
      entries.toVector
        .map(_.driver)
        .parUnorderedTraverse(_.terminate)
        .map(_.combineAll)

  def kill: IO[Unit] =
    stop

  def shutdownLocalSubagent(signal: Option[ProcessSignal], meta: CommandMeta): IO[Unit] =
    logger.traceIO:
      localSubagentDriver.flatMapSome: driver =>
        logger.debugIO:
          driver.shutdownSubagent(SubagentCommand.ShutDown(signal), meta).void
      .void

  private def localSubagentDriver: IO[Option[LocalSubagentDriver[S]]] =
    stateVar.value.map:
      _.idToAllocatedDriver.get(localSubagentId).map(_.allocatedThing).flatMap:
        case o: LocalSubagentDriver[S @unchecked] => Some(o)
        case o => throw new AssertionError(s"localSubagentDriver $o")

  def killLocalProcesses(signal: ProcessSignal): IO[Unit] =
    logger.debugIO:
      IO.defer:
        orderToSubagent.unsafeToMap.toVector.parFoldMapA:
          case (orderId, _: LocalSubagentDriver[?]) => killProcess(orderId, signal)
          case _ => IO.unit

  def stopWorkflowJobs(workflow: Workflow): IO[Unit] =
    stateVar.value.flatMap: state =>
      state.subagentToEntry.values.toVector.parFoldMapA:
        _.driver.stopWorkflowJobs(workflow)

  /** @return the persisted Order. */
  def processOrder(
    order: Order[IsFreshOrReady],
    endOfAdmissionPeriod: Option[Timestamp])
  : IO[Checked[Option[Order[Order.State]]]] =
    assertIfStrict(order.isProcessable)
    selectSubagentDriverCancelable(order).flatMap:
      case Left(problem) =>
        failProcessStart(problem, order)

      case Right(None) =>
        logger.debug(s"⚠️ ${order.id} has been cancelled while selecting a Subagent")
        IO.right(None)

      case Right(Some(selectedDriver)) =>
        processOrderAndForwardEvents(order, endOfAdmissionPeriod, selectedDriver)

  private def failProcessStart(problem: Problem, order: Order[IsFreshOrReady])
  : IO[Checked[Option[Order[Order.State]]]] =
    // Maybe suppress when this SubagentKeeper has been stopped ???
    // ExecuteExecutor should have prechecked this:
    val orderId = order.id
    journal.persist(Transaction):
      EventCalc.checked: agentState =>
        agentState.idToOrder.checked(orderId).map: order2 =>
          if order != order2 then
            // Do nothing when the Order has been changed by a concurrent event
            Nil
          else
            Vector[Option[OrderStarted | OrderProcessingStarted | OrderProcessed]](
              order2.isState[Order.Fresh] ? OrderStarted,
              // TODO Emit OrderFailedIntermediate_ instead, but this is not handled by this version
              Some(OrderProcessingStarted.noSubagent),
              Some(OrderProcessed(OrderOutcome.Disrupted(problem)))
            ).flatten.map(orderId <-: _)
    .flatTapT:
      onPersisted(orderId)
    .mapmap: persisted =>
      persisted.aggregate.idToOrder.get(orderId)

  private def processOrderAndForwardEvents(
    order: Order[IsFreshOrReady],
    endOfAdmissionPeriod: Option[Timestamp],
    selectedDriver: SelectedDriver)
  : IO[Checked[Option[Order[Order.State]]]] =
    // TODO Race with CancelOrders ?
    // TODO Race with stop of LocalSubagentDriver?
    //   May result in OrderProcessed(💥 Disrupted(ProcessLost(ServiceStopped: LocalSubagentDriver(..) service stopped)))
    import selectedDriver.{stick, subagentDriver}
    val orderId = order.id
    journal.persist[OrderStarted | OrderProcessingStarted]: agentState =>
      agentState.idToOrder.checked(orderId).map: currentOrder =>
        if currentOrder != order then
          // Do nothing when the Order has been changed by a concurrent operation
          // May happen when the same Order has been enqueued twice in JobMotor's queue
          // TODO Avoid this. ---> Let the OrderMotor pipeline emit OrderProcessingStarted <---
          if currentOrder.isState[Order.Processing] && order.isState[Order.Ready]
            && currentOrder.copy(state = order.state) == order
          then
            // This is because OrderMotor repeats because OrderProcessingStarted is
            // emitted asynchronously.
            // --> Let OrderMotor or ExecuteExecutor emit OrderProcessingStarted
            logger.trace(s"🪱 $orderId OrderProcessingStarted has already been emitted")
          else
            logger.debug:
              s"🪱 $orderId has concurrently been changed, no OrderProcessingStarted is emitted"
            logger.debug(s"Caller:  $order")
            logger.debug(s"Journal: $currentOrder")
          Nil
        else
          currentOrder.isState[Order.Fresh].thenList:
            orderId <-: OrderStarted
          .appended:
            orderId <-: OrderProcessingStarted(
              Some(subagentDriver.subagentId),
              selectedDriver.subagentBundleId.filter(_.toSubagentId != subagentDriver.subagentId),
              stick = stick,
              endOfAdmissionPeriod = endOfAdmissionPeriod)
    .flatTapT:
      onPersisted(orderId)
    .flatTapT: persisted =>
      if persisted.isEmpty then
        IO.right(())
      else
        IO.pure:
          persisted.aggregate.idToOrder.checked(orderId)
            .flatMap(_.checkedState[Order.Processing])
        .flatMapT: order =>
          forProcessingOrder(order, subagentDriver):
            subagentDriver.startOrderProcessing(order, endOfAdmissionPeriod)
        .handleError: t =>
          logger.error(s"processOrderAndForwardEvents $orderId => ${t.toStringWithCauses}",
            t.nullIfNoStackTrace)
          Left(Problem.fromThrowable(t))
        .requireElementType[Checked[FiberIO[OrderProcessed]]]
        .rightAs(())
    .mapmap: persisted =>
      persisted.nonEmpty thenMaybe:
        // Return the changed Order or None (then nothing has happened)
        persisted.aggregate.idToOrder.get(orderId)

  def recoverProcessingOrders(orders: Seq[Order[Order.Processing]])
  : IO[Seq[(OrderId, Checked[FiberIO[OrderProcessed]])]] =
    orders.parTraverse: order =>
      recoverOrderProcessing(order)
        .map(order.id -> _)

  // TODO recover all orders at once
  private def recoverOrderProcessing(order: Order[Order.Processing])
  : IO[Checked[FiberIO[OrderProcessed]]] =
    logger.traceIO("recoverOrderProcessing", order.id):
      val subagentId = order.state.subagentId getOrElse legacyLocalSubagentId
      stateVar.value
        .map:
          _.idToDriver.get(subagentId)
        .flatMap:
          case None =>
            val orderProcessed = OrderProcessed(OrderOutcome.Disrupted(Problem.pure:
              s"$subagentId is missed"))
            persist(order.id, orderProcessed :: Nil)
              .flatMapT: _ =>
                IO.pure(orderProcessed).start // dummy fiber
                  .map(Right(_))

          case Some(subagentDriver) =>
            forProcessingOrder(order, subagentDriver):
              if failedOverSubagentId contains subagentDriver.subagentId then
                subagentDriver.emitOrderProcessLostAfterRestart(order)
                  .flatMapT: orderProcessed =>
                    IO.pure(orderProcessed).start // dummy fiber
                      .map(Right(_))
              else
                subagentDriver.recoverOrderProcessing(order)
            .catchIntoChecked
            .flatTap:
              case Left(problem) =>
                IO(logger.error(s"recoverOrderProcessing ${order.id} => $problem"))
              case Right(_) => IO.unit

  private def persist[E <: OrderCoreEvent](orderId: OrderId, events: Seq[E])
  : IO[Checked[Persisted[S, E]]] =
    journal
      .persist(events.map(orderId <-: _))
      .flatTapT:
        onPersisted(orderId)

  private def onPersisted[E <: OrderCoreEvent](orderId: OrderId)
    (persisted: Persisted[S, E])
  : IO[Checked[Unit]] =
    persisted.aggregate.idToOrder.get(orderId) match
      case None =>
        logger.debug(s"❌ onPersisted ignored because $orderId has been deleted: ${
          persisted.keyedEvents.map(_.event.toShortString)}")
        IO.pure(Checked.unit)
      case Some(order) =>
        assert(persisted.keyedEvents.forall(_.key == orderId))
        callEventCallback(persisted)
          .map(Right(_))

  private def forProcessingOrder(order: Order[Order.Processing], subagentDriver: SubagentDriver)
    (body: IO[Checked[FiberIO[OrderProcessed]]])
  : IO[Checked[FiberIO[OrderProcessed]]] =
    val orderId = order.id
    orderToSubagent.put(orderId, subagentDriver) *>
      body.flatMapT: fiber =>
        // OrderProcessed event has been persisted by Local/RemoteSubagentDriver
        fiber.joinStd
          .flatTap: orderProcessed =>
            journal.aggregate.flatMap:
              _.idToOrder.get(orderId).fold(IO.right(())): order =>
                eventCallbackOnce.orThrow((orderId <-: orderProcessed) :: Nil)
          .guarantee:
            orderToSubagent.remove(orderId).void
          .start
          .map(Right(_))
      .guaranteeExceptWhenRight:
        orderToSubagent.remove(orderId).void

  private def callEventCallback[E <: Event](persisted: Persisted[S, E]): IO[Unit] =
    eventCallbackOnce.orThrow:
      persisted.keyedEvents.collect:
        case ke @ KeyedEvent(_, _: (OrderProcessingStarted | OrderProcessed)) =>
          ke.asInstanceOf[KeyedEvent[OrderProcessingStarted | OrderProcessed]]

  private def selectSubagentDriverCancelable(order: Order[IsFreshOrReady])
  : IO[Checked[Option[SelectedDriver]]] =
    orderToSubagentBundleId(order).flatMapT: determinedSubagentBundle =>
      import determinedSubagentBundle.{maybeSubagentBundleId, stick}
      cancelableWhileWaitingForSubagent(order.id).use: whenCancelled =>
        whenCancelled.get.race:
          selectSubagentDriver(maybeSubagentBundleId)
      .map:
        case Left(()) => Right(None) // Cancelled
        case Right(checkedDriver) =>
          checkedDriver.map: driver =>
            Some(SelectedDriver(maybeSubagentBundleId, driver, stick))

  private def orderToSubagentBundleId(order: Order[IsFreshOrReady])
  : IO[Checked[DeterminedSubagentBundle]] =
    for agentState <- journal.aggregate yield
      for
        job <- agentState.workflowJob(order.workflowPosition)
        scope <- agentState.toOrderScope(order)
        jobsBundleId <- job.subagentBundleId.traverse:
          _.evalAsString(using scope)
            .flatMap:
              case SubagentBundleId.LocalSubagentString =>
                Right(SubagentBundleId.fromSubagentId(localSubagentId))
              case string =>
                SubagentBundleId.checked(string)
      yield
        determineSubagentBundle(order, agentPath, jobsBundleId)

  /** While waiting for a Subagent, the Order is cancelable. */
  private def cancelableWhileWaitingForSubagent(orderId: OrderId): ResourceIO[Deferred[IO, Unit]] =
    Resource.eval:
      Deferred[IO, Unit]
    .flatTap: canceledDeferred =>
      Resource.make(
        acquire = IO:
          orderToWaitForSubagent.put(orderId, canceledDeferred))(
        release = _ => IO[Unit]:
          orderToWaitForSubagent.remove(orderId))

  private def selectSubagentDriver(maybeBundleId: Option[SubagentBundleId])
  : IO[Checked[SubagentDriver]] =
    val scope = maybeBundleId.foldMap(bundleProcessCountScope)
    Stream.repeatEval:
      stateVar.value.flatMap: directorState =>
        IO:
          def subagentIdToScope(subagentId: SubagentId) =
            maybeBundleId.fold(Scope.empty): bundleId =>
              bundleSubagentProcessCountScope(bundleId, subagentId)
          directorState.selectNext(maybeBundleId, scope, subagentIdToScope)
            //.tap(o => logger.trace(s"selectSubagentDriver($maybeBundleId) => $o ${stateVar.get}"))
    .evalTap:
      // TODO Do not poll (for each Order)
      case Right(None) => IO.sleep(reconnectDelayer.next())
      case _ => IO(reconnectDelayer.reset())
    .map(_.sequence)
    .flatMap(Stream.fromOption(_))
    .headL

  private def bundleProcessCountScope(bundleId: SubagentBundleId): Scope =
    new Scope:
      override def namedValue(name: String): Option[Checked[Value]] =
        name match
          case "js7ClusterProcessCount" =>
            Some(Right(NumberValue(bundleProcessCount(bundleId))))
          case _ => None

  private def bundleSubagentProcessCountScope(bundleId: SubagentBundleId, subagentId: SubagentId)
  : Scope =
    new Scope:
      override def namedValue(name: String): Option[Checked[Value]] =
        name match
          case "js7ClusterSubagentProcessCount" =>
            Some(Right(NumberValue(bundleSubagentProcessCount(bundleId, subagentId))))
          case _ => None

  /** Number of processes started via the specified SubagentBundleId.
    */
  private def bundleProcessCount(bundleId: SubagentBundleId): Int =
    val state = journal.unsafeAggregate()
    orderToSubagent.unsafeToMap.keysIterator
      .flatMap: orderId =>
        state.idToOrder.get(orderId)
      .count: order =>
        order.state match
          case state: Order.Processing => state.subagentBundleId.contains(bundleId)
          case _ => false

  /** Number of processes started via the specified SubagentBundleId and
    * running at the specified Subagent.
    */
  private def bundleSubagentProcessCount(bundleId: SubagentBundleId, subagentId: SubagentId): Int =
    val state = journal.unsafeAggregate()
    orderToSubagent.unsafeToMap.iterator
      .flatMap: (orderId, driver) =>
        (subagentId == driver.subagentId).thenMaybe:
          state.idToOrder.get(orderId)
      .count: order =>
        order.state match
          case state: Order.Processing => state.subagentBundleId.contains(bundleId)
          case _ => false

  def killProcess(orderId: OrderId, signal: ProcessSignal): IO[Unit] =
    // TODO Race condition?
    IO.defer:
      orderToWaitForSubagent.get(orderId).foldMap:
        _.complete(()).void
    .productR:
      IO.defer:
        orderToSubagent.get(orderId) match
          case None => IO(logger.error:
            s"killProcess($orderId): unexpected internal state: orderToSubagent does not contain the OrderId")
          case Some(driver) => driver.killProcess(orderId, signal)

  def resetAllSubagents(except: Set[SubagentId]): IO[Unit] =
    logger.traceIO:
      stateVar.value.flatMap: state =>
        state.subagentToEntry.values
          .toVector
          .map(_.driver)
          .collect:
            case driver: RemoteSubagentDriver[?] => driver
          .filterNot: driver =>
            except(driver.subagentId)
          .parUnorderedTraverse:
            _.reset(force = false, dontContinue = true)
          .map(_.combineAll)

  def startResetSubagent(subagentId: SubagentId, force: Boolean = false): IO[Checked[Unit]] =
    stateVar.value.map(_.idToDriver.checked(subagentId))
      .flatMapT:
        case driver: RemoteSubagentDriver[?] =>
          journal.persist(subagentId <-: SubagentResetStarted(force))
            .flatMapT: _ =>
              driver.reset(force)
                .handleError: t =>
                  logger.error(s"$subagentId reset => ${t.toStringWithCauses}", t.nullIfNoStackTrace)
                .startAndForget
                .map(Right(_))
        case _: LocalSubagentDriver[?] =>
          // Reset of the local Subagent (running in our JVM) would require a restart of the JVM
          IO.pure(Problem.pure(s"$subagentId as the Agent Director cannot be reset"))

  private def continueDetachingSubagents: IO[Unit] =
    journal.aggregate.flatMap(_
      .idToSubagentItemState.values
      .view
      .collect:
        case o if o.isDetaching => o.subagentId
      .toVector
      .parFoldMapA:
        removeSubagent)

  def removeSubagent(subagentId: SubagentId): IO[Unit] =
    logger.debugIO("removeSubagent", subagentId):
      stateVar.value.flatMap:
        _.idToDriver.get(subagentId).foldMap: subagentDriver =>
          subagentDriver.tryShutdownForRemoval // may take a short time
            .productR:
              stateVar.updateDirect: state =>
                state.removeSubagent(subagentId)
            .productR:
              subagentDriver.terminate
        .productR:
          journal.persistTransaction[OrderProcessed | ItemDetached]: state =>
            Right:
              state.idToOrder.values.view
                .flatMap(_.ifProcessing(subagentId))
                .map:
                  // Not OrderOutcome.processLost because the job should not be repeated,
                  // in case it requests the same, now deleted, Subagent.
                  // Also, this has been this way for years.
                  // User must be sure that the Subagent is dead !!!
                  _.id <-: OrderProcessed(OrderOutcome.Disrupted:
                    ProcessLostDueSubagentDeletedProblem(subagentId))
                .concat:
                  // Check again to be sure
                  state.idToSubagentItemState.get(subagentId).exists(_.isDetaching) ?
                    ItemDetached(subagentId, agentPath)
          .ifPersisted:
            callEventCallback
          .orThrow.void

  private def recoverSubagents(subagentItemStates: Seq[SubagentItemState]): IO[Checked[Unit]] =
    subagentItemStates
      .traverse: state =>
        addOrChange(state)
          .map(_.map(_.map(_ => state)))
      .map(_.combineProblems)
      .map(_.map(_.flatten))

  private def startObserving: IO[Unit] =
    stateVar.value.flatMap:
      _.subagentToEntry.values.map(_.driver).foldMap:
        _.startObserving

  private def recoverSubagentBundles(subagentBundles: Seq[SubagentBundle]): IO[Checked[Unit]] =
    logger.debugIO:
      subagentBundles.foldMap(addOrReplaceSubagentBundle)

  // TODO Kann SubagentItem gelöscht werden während proceed hängt wegen unerreichbaren Subagenten?
  def proceedWithSubagent(subagentItemState: SubagentItemState): IO[Checked[Unit]] =
    logger.traceIO("proceedWithSubagent", subagentItemState.pathRev)(
      addOrChange(subagentItemState)
        .rightAs(()))

  // May return a new, non-started RemoteSubagentDriver
  private def addOrChange(subagentItemState: SubagentItemState)
  : IO[Checked[Option[SubagentDriver]]] =
    logger.debugIO("addOrChange", subagentItemState.pathRev):
      val subagentItem = subagentItemState.subagentItem
      stateVar.value
        .map(_.idToDriver.get(subagentItem.id))
        .flatMap:
          case Some(_: LocalSubagentDriver[?]) =>
            stateVar
              .updateChecked(state => IO:
                state.setDisabled(subagentItem.id, subagentItem.disabled))
              .rightAs(None)

          case _ =>
            // Don't use the matched RemoteAgentDriver. We update state with an atomic operation.
            stateVar.updateCheckedWithResult: state =>
              state.idToAllocatedDriver.get(subagentItem.id) match
                case None =>
                  allocateSubagentDriver(subagentItem)
                    .map: allocatedDriver =>
                      state.insertSubagentDriver(allocatedDriver, disabled = subagentItem.disabled)
                        .flatMap(_.setDisabled(subagentItem.id, subagentItem.disabled))
                        .map(_ -> Some(None -> allocatedDriver.allocatedThing))

                case Some(existingAllo) =>
                  val existingAllocated = existingAllo
                    .asInstanceOf[Allocated[IO, SubagentDriver]]
                  val existingDriver = existingAllocated.allocatedThing
                  IO.defer:
                    if subagentItem.uri == existingDriver.subagentItem.uri then
                      IO.right(state -> None)
                    else
                      // Subagent moved
                      remoteSubagentDriverResource(subagentItem)
                        .toAllocated
                        .map: allocatedDriver =>
                          state.replaceSubagentDriver(allocatedDriver, subagentItem)
                            .map(_ -> Some(Some(existingAllocated) -> allocatedDriver.allocatedThing))
                  // Continue after locking updateCheckedWithResult )
                  .flatMapT: (state, result) =>
                    IO(state
                      .setDisabled(subagentItem.id, subagentItem.disabled)
                      .map(_ -> result))
        .flatMapT:
          case Some((Some(Allocated(
              oldDriver: RemoteSubagentDriver[S @unchecked], releaseOld)),
              newDriver: RemoteSubagentDriver[S @unchecked]
            )) =>
            assert(oldDriver.subagentId == newDriver.subagentId)
            val name = "addOrChange " + oldDriver.subagentItem.pathRev
            oldDriver
              .stopDispatcherAndEmitProcessLostEvents(ProcessLostDueSubagentUriChangeProblem, None)
              .*>(releaseOld)  // Maybe try to send Shutdown command ???
              .*>(subagentItemLockKeeper
                .lock(oldDriver.subagentId):
                  newDriver.startMovedSubagent(oldDriver)
                .logWhenItTakesLonger(name)
                .handleError(t => logger.error(
                  s"addOrChange $name => ${t.toStringWithCauses}", t.nullIfNoStackTrace))
                .startAndForget
                .as(Right(None)))

          case Some((None, newDriver: LocalSubagentDriver[?])) =>
            emitLocalSubagentCoupled
              .as(Right(Some(newDriver)))

          case maybeNewDriver => IO.right(maybeNewDriver.map(_._2))

  private def emitLocalSubagentCoupled: IO[Unit] =
    journal.persist:
      _.idToSubagentItemState.get(localSubagentId)
        .exists(_.couplingState != Coupled)
        .thenList:
          localSubagentId <-: SubagentCoupled
    .orThrow
    .void

  private def allocateSubagentDriver(subagentItem: SubagentItem) =
    if subagentItem.id == localSubagentId then
      emitLocalSubagentCoupled *>
        localSubagentDriverResource(subagentItem).toAllocated
    else
      remoteSubagentDriverResource(subagentItem).toAllocated

  private def localSubagentDriverResource(subagentItem: SubagentItem): ResourceIO[SubagentDriver] =
    LocalSubagentDriver
      .service(subagentItem, localSubagent, journal, controllerId,
        directorConf.subagentConf)
      .evalTap: driver =>
        IO.whenA(started):
          driver.startObserving

  private def remoteSubagentDriverResource(subagentItem: SubagentItem)
  : ResourceIO[RemoteSubagentDriver[S]] =
    for
      api <- subagentApiResource(subagentItem)
      driver <- RemoteSubagentDriver
        .service(subagentItem, api, journal, controllerId,
          directorConf.subagentDriverConf, directorConf.recouplingStreamReaderConf)
        .evalTap: driver =>
          IO.whenA(started):
            driver.startObserving
    yield
      driver

  private def subagentApiResource(subagentItem: SubagentItem): ResourceIO[HttpSubagentApi] =
    assertThat(subagentItem.id != localSubagentId)
    HttpSubagentApi.resource(
      Admission(
        subagentItem.uri,
        directorConf.config
          .optionAs[SecretString]:
            "js7.auth.subagents." + ConfigUtil.joinPath(subagentItem.id.string)
          .map:
            UserAndPassword(localSubagentId.toUserId.orThrow, _)),
      directorConf.httpsConfig,
      name = subagentItem.id.toString)
      (using actorSystem)

  def addOrReplaceSubagentBundle(bundle: SubagentBundle): IO[Checked[Unit]] =
    stateVar
      .updateChecked(state => IO:
        state.insertOrReplaceBundle(bundle))
      .rightAs(())

  def removeSubagentBundle(subagentBundleId: SubagentBundleId): IO[Unit] =
    stateVar
      .update(state => IO(state.removeBundle(subagentBundleId)))
      .void

  def testFailover(): Unit =
    stateVar.get.idToDriver.values
      .collect:
        case o: LocalSubagentDriver[?] => o
      .foreach:
        _.testFailover()

  override def toString = s"SubagentKeeper(${orderToSubagent.size} processing orders)"


object SubagentKeeper:
  type EventCallback = Iterable[KeyedEvent[OrderProcessingStarted | OrderProcessed]] => IO[Unit]

  private val logger = Logger[this.type]

  def service[S <: SubagentDirectorState[S]](
    localSubagentId: SubagentId,
    localSubagent: Subagent,
    agentPath: AgentPath,
    controllerId: ControllerId,
    failedOverSubagentId: Option[SubagentId],
    journal: Journal[S],
    directorConf: DirectorConf,
    actorSystem: ActorSystem)
    (using IORuntime, Tag[S])
  : ResourceIO[SubagentKeeper[S]] =
    Service.resource:
      new SubagentKeeper(localSubagentId, localSubagent, agentPath, controllerId,
        failedOverSubagentId, journal, directorConf, actorSystem)

  private[director] def determineSubagentBundle(
    order: Order[IsFreshOrReady],
    agentPath: AgentPath,
    maybeJobsBundleId: Option[SubagentBundleId])
  : DeterminedSubagentBundle =
    order.agentToStickySubagent(agentPath) match
      case Some(sticky)
        if maybeJobsBundleId.forall(o => sticky.subagentBundleId.forall(_ == o)) =>
        // StickySubagent instruction applies
        DeterminedSubagentBundle(
          sticky.stuckSubagentId
            .map(SubagentBundleId.fromSubagentId)
            .orElse(sticky.subagentBundleId)
            .orElse(maybeJobsBundleId),
          stick = sticky.stuckSubagentId.isEmpty)

      case _ =>
        DeterminedSubagentBundle(maybeJobsBundleId)


  private final case class SelectedDriver(
    subagentBundleId: Option[SubagentBundleId],
    subagentDriver: SubagentDriver,
    stick: Boolean)

  private[director] final case class DeterminedSubagentBundle(
    maybeSubagentBundleId: Option[SubagentBundleId],
    stick: Boolean = false)

  private[director] object DeterminedSubagentBundle:
    @TestOnly
    def stuck(stuckSubagentId: SubagentId): DeterminedSubagentBundle =
      DeterminedSubagentBundle(Some(SubagentBundleId.fromSubagentId(stuckSubagentId)))
