package js7.subagent.director

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, FiberIO, IO, Resource, ResourceIO}
import cats.implicits.catsSyntaxParallelUnorderedTraverse
import cats.instances.option.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import com.typesafe.config.ConfigUtil
import fs2.{Chunk, Stream}
import izumi.reflect.Tag
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.catsutils.CatsEffectExtensions.{guaranteeExceptWhenRight, joinStd, left, materializeIntoChecked, orThrow, right, startAndForget}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.generic.SecretString
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.headL
import js7.base.monixutils.{AsyncMap, AsyncVariable}
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.{DelayIterator, DelayIterators}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, LockKeeper, StandardMapView}
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.delegate.DelegateCouplingState.Coupled
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.item.BasicItemEvent.ItemDetached
import js7.data.job.JobKey
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.subagent.Problems.ProcessLostDueSubagentUriChangeProblem
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentResetStarted}
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentDirectorState, SubagentId, SubagentItem, SubagentItemState}
import js7.data.value.expression.Scope
import js7.data.value.{NumberValue, Value}
import js7.journal.state.Journal
import js7.subagent.Subagent
import js7.subagent.configuration.DirectorConf
import js7.subagent.director.SubagentKeeper.*
import org.apache.pekko.actor.ActorSystem
import org.jetbrains.annotations.TestOnly

final class SubagentKeeper[S <: SubagentDirectorState[S]: Tag](
  localSubagentId: SubagentId,
  localSubagent: Subagent,
  agentPath: AgentPath,
  controllerId: ControllerId,
  failedOverSubagentId: Option[SubagentId],
  journal: Journal[S],
  directorConf: DirectorConf,
  actorSystem: ActorSystem)
  (implicit ioRuntime: IORuntime):

  private val reconnectDelayer: DelayIterator = DelayIterators
    .fromConfig(directorConf.config, "js7.subagent-driver.reconnect-delays")(
      using ioRuntime.scheduler)
    .orThrow
  private lazy val legacyLocalSubagentId = SubagentId.legacyLocalFromAgentPath(agentPath) // COMPATIBLE with v2.2
  private val stateVar = AsyncVariable(DirectorState.initial(directorConf))
  private val orderToWaitForSubagent = AsyncMap.empty[OrderId, Deferred[IO, Unit]]
  private val orderToSubagent = AsyncMap.empty[OrderId, SubagentDriver]
  private val subagentItemLockKeeper = new LockKeeper[SubagentId]
  @volatile private var started = false // Delays SubagentDriver#startObserving

  def stop: IO[Unit] =
    logger.traceIO:
      stateVar
        .updateWithResult(state => IO:
          state.clear -> state.subagentToEntry.values)
        .flatMap: entries =>
          entries.toVector
            .map(_.driver)
            .parUnorderedTraverse(_.terminate)
            .map(_.combineAll)

  // Call this, but not before recovering !!!
  def start: IO[Unit] =
    logger.traceIO:
      IO.defer:
        started = true
        startObserving *> continueDetaching

  def stopJobs(jobKeys: Iterable[JobKey], signal: ProcessSignal): IO[Unit] =
    stateVar.value.flatMap: state =>
      state.subagentToEntry.values.toVector
        .parUnorderedTraverse(_.driver.stopJobs(jobKeys, signal))
        .map(_.combineAll)

  def orderIsLocal(orderId: OrderId): Boolean =
    orderToSubagent.toMap.get(orderId).exists(_.isInstanceOf[LocalSubagentDriver])

  def processOrder(
    order: Order[Order.IsFreshOrReady],
    onEvents: Seq[OrderCoreEvent] => Unit)
  : IO[Checked[Unit]] =
    selectSubagentDriverCancelable(order)
      .flatMap:
        case Left(problem) =>
          // Maybe suppress when this SubagentKeeper has been stopped ???
          IO.defer:
            // ExecuteExecutor should have prechecked this:
            val events = order.isState[Order.Fresh].thenList(OrderStarted) :::
              // TODO Emit OrderFailedIntermediate_ instead, but this is not handled by this version
              OrderProcessingStarted.noSubagent ::
              OrderProcessed(OrderOutcome.Disrupted(problem)) :: Nil
            persist(order.id, events, onEvents)
              .rightAs(())

        case Right(None) =>
          logger.debug(s"⚠️ ${order.id} has been canceled while selecting a Subagent")
          IO.right(())

        case Right(Some(selectedDriver)) =>
          processOrderAndForwardEvents(order, onEvents, selectedDriver)

  private def processOrderAndForwardEvents(
    order: Order[Order.IsFreshOrReady],
    onEvents: Seq[OrderCoreEvent] => Unit,
    selectedDriver: SelectedDriver)
  : IO[Checked[Unit]] =
    // TODO Race with CancelOrders ?
    import selectedDriver.{stick, subagentDriver}

    val events = order.isState[Order.Fresh].thenList(OrderStarted) :::
      OrderProcessingStarted(
        Some(subagentDriver.subagentId),
        selectedDriver.subagentBundleId.filter(_.toSubagentId != subagentDriver.subagentId),
        stick = stick) ::
      Nil
    persist(order.id, events, onEvents)
      .map(_.map: (_, s) =>
        s.idToOrder
          .checked(order.id)
          .flatMap(_.checkedState[Order.Processing])
          .orThrow)
      .flatMapT: order =>
        forProcessingOrder(order.id, subagentDriver, onEvents)(
          subagentDriver.startOrderProcessing(order))
      .handleErrorWith(t => IO:
        logger.error(s"processOrderAndForwardEvents ${order.id} => ${t.toStringWithCauses}",
          t.nullIfNoStackTrace)
        Left(Problem.fromThrowable(t)))
      .containsType[Checked[FiberIO[OrderProcessed]]]
      .rightAs(())

  def recoverOrderProcessing(
    order: Order[Order.Processing],
    onEvents: Seq[OrderCoreEvent] => Unit)
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
            persist(order.id, orderProcessed :: Nil, onEvents)
              .flatMapT(_ => IO.pure(orderProcessed).start.map(Right(_)))

          case Some(subagentDriver) =>
            forProcessingOrder(order.id, subagentDriver, onEvents):
              if failedOverSubagentId contains subagentDriver.subagentId then
                subagentDriver.emitOrderProcessLostAfterRestart(order)
                  .flatMap(_.traverse(orderProcessed => IO.pure(orderProcessed).start))
              else
                subagentDriver.recoverOrderProcessing(order)
            .materializeIntoChecked
            .flatTap:
              case Left(problem) => IO(logger.error:
                s"recoverOrderProcessing ${order.id} => $problem")
              case Right(_) => IO.unit

  private def persist(
    orderId: OrderId,
    events: Seq[OrderCoreEvent],
    onEvents: Seq[OrderCoreEvent] => Unit)
  : IO[Checked[(Seq[Stamped[KeyedEvent[OrderCoreEvent]]], S)]] =
    journal
      .persistKeyedEvents(events.map(orderId <-: _))
      .map(_.map: o =>
        onEvents(events)
        o)

  private def forProcessingOrder(
    orderId: OrderId,
    subagentDriver: SubagentDriver,
    onEvents: Seq[OrderCoreEvent] => Unit)
    (body: IO[Checked[FiberIO[OrderProcessed]]])
  : IO[Checked[FiberIO[OrderProcessed]]] =
    val release = orderToSubagent.remove(orderId).void
    orderToSubagent
      .put(orderId, subagentDriver)
      .*>(body
        .flatMap:
          case Left(problem) => IO.left(problem)
          case Right(fiber) =>
            // OrderProcessed event has been persisted by RemoteSubagentDriver
            fiber.joinStd
              .map: orderProcessed =>
                onEvents(orderProcessed :: Nil)
                orderProcessed
              .guarantee(release)
              .start
              .map(Right(_)))
      .guaranteeExceptWhenRight(release)

  private def selectSubagentDriverCancelable(order: Order[Order.IsFreshOrReady])
  : IO[Checked[Option[SelectedDriver]]] =
    orderToSubagentBundleId(order).flatMapT:
      case DeterminedSubagentBundle(subagentBundleId, stick) =>
        cancelableWhileWaitingForSubagent(order.id)
          .use: canceledPromise =>
            IO.race(
              canceledPromise.get,
              selectSubagentDriver(subagentBundleId))
          .map(_
            .toOption
            .sequence
            .map(_.map(SelectedDriver(subagentBundleId, _, stick))))

  private def orderToSubagentBundleId(order: Order[Order.IsFreshOrReady])
  : IO[Checked[DeterminedSubagentBundle]] =
    for agentState <- journal.state yield
      for
        job <- agentState.workflowJob(order.workflowPosition)
        scope <- agentState.toPureOrderScope(order)
        jobsBundleId <- job.subagentBundleId
          .traverse(_.evalAsString(scope)
          .flatMap(SubagentBundleId.checked))
      yield
        determineSubagentBundle(order, agentPath, jobsBundleId)

  /** While waiting for a Subagent, the Order is cancelable. */
  private def cancelableWhileWaitingForSubagent(orderId: OrderId)
  : ResourceIO[Deferred[IO, Unit]] =
    Resource
      .eval(Deferred[IO, Unit])
      .flatMap: canceledDeferred =>
        Resource.make(
          acquire = orderToWaitForSubagent.put(orderId, canceledDeferred))(
          release = _ => orderToWaitForSubagent.remove(orderId).void)

  private def selectSubagentDriver(maybeBundleId: Option[SubagentBundleId])
  : IO[Checked[SubagentDriver]] =
    logger.traceIO:
      val scope = maybeBundleId.fold(Scope.empty)(bundleSubagentProcessCountScope)
      Stream
        .repeatEval:
          stateVar.value
            .flatMap(directorState => IO:
              directorState.selectNext(maybeBundleId, scope))
            .flatTap(o => IO:
              logger.trace(s"selectSubagentDriver($maybeBundleId) => $o ${stateVar.get}"))
        .evalTap:
          // TODO Do not poll (for each Order)
          case Right(None) => IO.sleep(reconnectDelayer.next())
          case _ => IO(reconnectDelayer.reset())
        .map(_.sequence)
        .map(Chunk.fromOption)
        .unchunks
        .headL

  private def bundleSubagentProcessCountScope(bundleId: SubagentBundleId) =
    val Key = "js7BundleSubagentProcessCount"
    new Scope:
      override def nameToCheckedValue =
        new StandardMapView[String, Checked[Value]]:
          override val keySet = Set(Key)

          override def get(key: String) =
            key match
              case Key => Some(Right(NumberValue(bundleSubagentProcessCount(bundleId))))
              case _ => None

  private def bundleSubagentProcessCount(bundleId: SubagentBundleId): Int =
    val state = journal.unsafeCurrentState()
    orderToSubagent.toMap.keys.view
      .flatMap:
        state.idToOrder.get
      .flatMap:
        _.ifState[Order.Processing]
      .count:
        _.state.subagentBundleId contains bundleId

  def killProcess(orderId: OrderId, signal: ProcessSignal): IO[Unit] =
    IO.defer:
      // TODO Race condition?
      orderToWaitForSubagent
        .get(orderId)
        .fold(IO.unit)(_.complete(()))
        .*>(orderToSubagent
          .get(orderId) match
            case None => IO(logger.error:
              s"killProcess($orderId): unexpected internal state: orderToSubagent does not contain the OrderId")
            case Some(driver) => driver.killProcess(orderId, signal))

  def resetAllSubagents(except: Set[SubagentId]): IO[Unit] =
    logger.traceIO:
      stateVar.value
        .flatMap: state =>
          state.subagentToEntry.values
            .toVector
            .map(_.driver)
            .collect:
              case driver: RemoteSubagentDriver => driver
            .filterNot: driver =>
              except(driver.subagentId)
            .parUnorderedTraverse:
              _.reset(force = false, dontContinue = true)
            .map(_.combineAll)

  def startResetSubagent(subagentId: SubagentId, force: Boolean = false): IO[Checked[Unit]] =
    stateVar.value
      .flatMap(s => IO:
        s.idToDriver.checked(subagentId))
      .flatMapT:
        case driver: RemoteSubagentDriver =>
          journal.persistKeyedEvent(subagentId <-: SubagentResetStarted(force))
            .flatMapT: _ =>
              driver.reset(force)
                .handleError: t =>
                  logger.error(s"$subagentId reset => ${t.toStringWithCauses}", t.nullIfNoStackTrace)
                .startAndForget
                .map(Right(_))
        case _: LocalSubagentDriver =>
          // Reset of the local Subagent (running in our JVM) would require a restart of the JVM
          IO.pure(Problem.pure(s"$subagentId as the Agent Director cannot be reset"))

  def startRemoveSubagent(subagentId: SubagentId): IO[Unit] =
    removeSubagent(subagentId)
      .handleError[Unit]: t =>
        logger.error(s"removeSubagent($subagentId) => $t")
      .startAndForget

  private def removeSubagent(subagentId: SubagentId): IO[Unit] =
    logger.debugIO("removeSubagent", subagentId):
      stateVar.value
        .flatMap(_.idToDriver
          .get(subagentId)
          .fold(IO.unit): subagentDriver =>
            subagentDriver.tryShutdown
              .*>(stateVar.update(state => IO:
                state.removeSubagent(subagentId)))
              .*>(subagentDriver.terminate))
        .*>(journal
          .persistKeyedEvent(ItemDetached(subagentId, agentPath))
          .orThrow
          .void)

  def recoverSubagents(subagentItemStates: Seq[SubagentItemState]): IO[Checked[Unit]] =
    subagentItemStates
      .traverse: state =>
        addOrChange(state)
          .map(_.map(_.map(_ => state)))
      .map(_.combineProblems)
      .map(_.map(_.flatten))

  private def startObserving: IO[Unit] =
    stateVar.value
      .flatMap:
        _.subagentToEntry.values.toVector.map(_.driver).traverse:
          _.startObserving
      .map(_.combineAll)

  private def continueDetaching: IO[Unit] =
    journal.state.flatMap(_
      .idToSubagentItemState.values
      .view
      .collect:
        case o if o.isDetaching => o.subagentId
      .toVector
      .traverse(startRemoveSubagent)
      .map(_.combineAll))

  def recoverSubagentBundles(subagentBundles: Seq[SubagentBundle]): IO[Checked[Unit]] =
    logger.debugIO:
      subagentBundles
        .traverse(addOrReplaceSubagentBundle)
        .map(_.combineAll)

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
          case Some(_: LocalSubagentDriver) =>
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
          case Some((Some(Allocated(oldDriver: RemoteSubagentDriver, releaseOld)), newDriver: RemoteSubagentDriver)) =>
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

          case Some((None, newDriver: LocalSubagentDriver)) =>
            emitLocalSubagentCoupled
              .as(Right(Some(newDriver)))

          case maybeNewDriver => IO.right(maybeNewDriver.map(_._2))

  private def emitLocalSubagentCoupled: IO[Unit] =
    journal
      .persist: agentState =>
        Right:
          agentState
            .idToSubagentItemState.get(localSubagentId)
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

  private def localSubagentDriverResource(subagentItem: SubagentItem)
  : ResourceIO[SubagentDriver] =
    LocalSubagentDriver
      .resource(
        subagentItem,
        localSubagent,
        journal,
        controllerId,
        directorConf.subagentConf)
      .evalTap: driver =>
        IO.whenA(started):
          driver.startObserving

  private def remoteSubagentDriverResource(subagentItem: SubagentItem)
  : ResourceIO[RemoteSubagentDriver] =
    for
      api <- subagentApiResource(subagentItem)
      driver <- RemoteSubagentDriver
        .resource(
          subagentItem,
          api,
          journal,
          controllerId,
          directorConf.subagentDriverConf,
          directorConf.recouplingStreamReaderConf)
        .evalTap(driver => IO.whenA(started):
          driver.startObserving)
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
      name = subagentItem.id.toString,
      actorSystem)

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
        case o: LocalSubagentDriver => o
      .foreach:
        _.testFailover()

  override def toString = s"SubagentKeeper(${orderToSubagent.size} processing orders)"


object SubagentKeeper:
  private val logger = Logger[this.type]

  private[director] def determineSubagentBundle(
    order: Order[Order.IsFreshOrReady],
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
