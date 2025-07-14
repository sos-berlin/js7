package js7.agent.motor

import cats.effect.std.Dispatcher
import cats.effect.unsafe.IORuntime
import cats.effect.{FiberIO, IO, Ref, Resource, ResourceIO}
import cats.syntax.flatMap.*
import cats.syntax.traverse.*
import io.circe.syntax.EncoderOps
import java.time.ZoneId
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachItem, AttachOrder, AttachSignedItem, DetachItem, DetachOrder, MarkOrder, OrderCommand, ReleaseEvents, ResetSubagent, Response}
import js7.agent.data.event.AgentEvent.{AgentReady, AgentShutDown}
import js7.agent.motor.AgentMotor.*
import js7.base.catsutils.CatsEffectExtensions.{catchIntoChecked, joinStd, left, right, startAndForget, startAndLogError}
import js7.base.catsutils.CatsEffectUtils.whenDeferred
import js7.base.catsutils.CatsExtensions.flatMapSome
import js7.base.catsutils.Environment.environment
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.crypt.Signed
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, Logger}
import js7.base.monixutils.{AsyncMap, AsyncVariable}
import js7.base.problem.Checked.{Ops, RichCheckedF}
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.time.JavaTime.extensions.toZoneId
import js7.base.time.ScalaTime.*
import js7.base.time.{AlarmClock, Timestamp}
import js7.base.utils.Atomic
import js7.base.utils.CatsUtils.pureFiberIO
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.cluster.WorkingClusterNode
import js7.common.system.PlatformInfos.currentPlatformInfo
import js7.core.problems.ReverseReleaseEventsProblem
import js7.data.Problems.PassiveClusterNodeUrlChangeableOnlyWhenNotCoupledProblem
import js7.data.agent.Problems.{AgentDuplicateOrder, AgentIsShuttingDown}
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.calendar.Calendar
import js7.data.cluster.ClusterState
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.controller.ControllerId
import js7.data.event.JournalEvent.JournalEventsReleased
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventCalc, EventId, KeyedEvent, TimeCtx}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.item.BasicItemEvent.{ItemAttachedToMe, ItemDetached, ItemDetachingFromMe, SignedItemAttachedToMe}
import js7.data.item.{InventoryItem, InventoryItemEvent, InventoryItemKey, SignableItem, UnsignedItem}
import js7.data.job.JobResource
import js7.data.node.NodeId
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAttachedToAgent, OrderDetached, OrderForked, OrderKillingMarked, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{Order, OrderEvent, OrderId, OrderMark}
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentId, SubagentItem}
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowId, WorkflowPathControl}
import js7.journal.Persisted
import js7.launcher.configuration.Problems.SignedInjectionNotAllowed
import js7.subagent.Subagent
import js7.subagent.director.SubagentKeeper
import org.apache.pekko.actor.ActorSystem
import scala.concurrent.duration.Deadline.now

/** The dedicated Director.
  *
  * The Director runs Orders.
  * For Job execution, the Director delegates Orders via the SubagentKeeper to a number of
  * Subagents.
  */
final class AgentMotor private(
  failedOverSubagentId: Option[SubagentId],
  val controllerId: ControllerId,
  val agentPath: AgentPath,
  localSubagentId: SubagentId,
  subagentKeeper: SubagentKeeper[AgentState],
  forDirector: Subagent.ForDirector,
  fileWatchManager: FileWatchManager,
  workingClusterNode: WorkingClusterNode[AgentState],
  conf: AgentConfiguration)
  (using
    clock: AlarmClock,
    dispatcher: Dispatcher[IO])
extends Service.StoppableByRequest:

  private val journal = workingClusterNode.journal
  private val orderToEntry = AsyncMap[OrderId, OrderEntry]
  private val jobMotor = JobMotor(agentPath, subagentKeeper, journal.aggregate, onSubagentEvents,
    isStopping = isStopping, conf)
  private val agentProcessCount = Atomic(0)
  private val _stopImmediately = Ref.unsafe[IO, Boolean](false)
  private val _shutdown = Ref.unsafe[IO, Option[AgentCommand.ShutDown]](None)
  /** When only the Director should be shut down while the Subagent keeps running. */
  private val _inhibitShutdownLocalSubagent = Ref.unsafe[IO, Boolean](false)

  protected def start =
    journal.aggregate.flatMap: agentState =>
      recoverJobEntries(agentState) *>
        journal.persist:
          AgentReady(
            ZoneId.systemDefault.getId,
            totalRunningTime = journal.totalRunningTime,
            Some(currentPlatformInfo()))
        .map(_.orThrow) *>
        recoverOrders(agentState)
      *>
        startService:
          untilStopRequested *> stopMe

  private def stopMe: IO[Unit] =
    IO.defer:
      orderToEntry.toMap.values.foldMap(_.stop) *>
        jobMotor.stop *>
        _stopImmediately.get.flatMap:
          IO.unlessA(_):
            _inhibitShutdownLocalSubagent.get.flatMap:
              IO.unlessA(_):
                _shutdown.get.map(_.flatMap(_.processSignal)).flatMap: maybeSignal =>
                  subagentKeeper.shutdownLocalSubagent(maybeSignal)
            *>
              persist(AgentShutDown)
                .handleProblemWith: problem =>
                  IO(logger.error(s"AgentShutDown: $problem"))
                .void

  def kill: IO[Unit] =
    logger.debugIO:
      _stopImmediately.set(true) *>
        //subagentKeeper.kill *>
        stop *> subagentKeeper.kill *> journal.kill

  private def recoverJobEntries(agentState: AgentState): IO[Unit] =
    agentState.idToWorkflow.values.foldMap:
      jobMotor.createJobEntries

  private def recoverOrders(agentState: AgentState): IO[Unit] =
    val orders = agentState.idToOrder.values.view
    val processingOrders = orders.flatMap(_.ifState[Order.Processing]).toVector
    val otherOrders = orders.filterNot(_.isState[Order.Processing]).toVector
    // Continue already processing Orders
    jobMotor.recoverProcessingOrders(processingOrders).flatMap: checkedFibers =>
      checkedFibers.foldMap: (orderId, checkedFiber) =>
        checkedFiber.traverse: fiber =>
          fiber.joinStd.flatMap: (_: OrderProcessed) =>
            proceedWithOrder(orderId)
          .handleErrorWith: t =>
            IO(logger.error(s"recoverOrderProcessing($orderId): ${t.toStringWithCauses}", t))
          .startAndForget
        .handleProblemWith: problem =>
          IO(logger.error(s"recoverOrderProcessing($orderId): $problem"))
    .productR:
      proceedWithOrders(otherOrders)

  def executeCommand(cmd: AgentCommand): IO[Checked[AgentCommand.Response]] =
    cmd match
      case cmd: OrderCommand =>
        executeOrderCommand(cmd)

      case AttachItem(item) =>
        attachUnsignedItem(item)

      case AttachSignedItem(signed) =>
        attachSignedItem(signed)

      case DetachItem(itemKey) =>
        detachItem(itemKey)

      case ResetSubagent(subagentId, force) =>
        subagentKeeper.startResetSubagent(subagentId, force)
          .rightAs(AgentCommand.Response.Accepted)

      case _ => IO.left(Problem(s"Unknown command: ${cmd.getClass.simpleScalaName}"))

  private def executeOrderCommand(cmd: OrderCommand): IO[Checked[AgentCommand.Response]] =
    cmd match
      case AttachOrder(order) =>
        if isStopping then
          IO.pure(Left(AgentIsShuttingDown))
        else
          order.attached match
            case Left(problem) => IO.left(problem)
            case Right(agentPath) =>
              if agentPath != this.agentPath then
                IO.left(Problem(s"Wrong $agentPath"))
              else
                persist: agentState =>
                  agentState.idToWorkflow.checked(order.workflowId).flatMap: workflow =>
                    if !workflow.isDefinedAt(order.position) then
                      Left(Problem.pure(s"Unknown Position ${order.workflowPosition}"))
                    else if agentState.idToOrder.contains(order.id) then
                      Left(AgentDuplicateOrder(order.id))
                    else if isStopping then
                      Left(AgentIsShuttingDown)
                    else
                      order.toOrderAttachedToAgent.map: event =>
                        Some(order.id <-: event)
                .rightAs(Response.Accepted)

      case DetachOrder(orderId) =>
        if isStopping then
          IO.pure(Left(AgentIsShuttingDown))
        else
          persist: agentState =>
            agentState.idToOrder.get(orderId).fold(Right(Nil)): order =>
              order.detaching.map: _ =>
                (orderId <-: OrderDetached) :: Nil
          .rightAs(AgentCommand.Response.Accepted)

      case MarkOrder(orderId, mark) =>
        persist: agentState =>
          agentState.idToOrder.checked(orderId).flatMap: order =>
            if order.isDetaching then
              Right(Nil)
            else
              OrderEventSource(agentState)(using InstructionExecutorService(clock))
                .markOrder(orderId, mark).map: events =>
                  events.map(orderId <-: _)
        .rightAs(AgentCommand.Response.Accepted)

      case ReleaseEvents(after) =>
        if isStopping then
          IO.pure(Left(AgentIsShuttingDown))
        else
          val userId = controllerId.toUserId
          persist: agentState =>
            val current = agentState.journalState.userIdToReleasedEventId(userId) // Must contain userId
            if after < current then
              Left(ReverseReleaseEventsProblem(
                requestedUntilEventId = after, currentUntilEventId = current))
            else
              Right:
                (NoKey <-: JournalEventsReleased(userId, after)) :: Nil
          .rightAs(AgentCommand.Response.Accepted)

  private def attachUnsignedItem(item: UnsignedItem): IO[Checked[Response.Accepted]] =
    item match
      case agentRef: AgentRef =>
        if agentRef.path != agentPath then
          IO.left(Problem(s"Alien AgentRef(${agentRef.path})"))
        else
          changeSubagentAndClusterNodeThenProceed(ItemAttachedToMe(agentRef))
            .rightAs(AgentCommand.Response.Accepted)

      case item: SubagentItem =>
        changeSubagentAndClusterNodeThenProceed(ItemAttachedToMe(item))
          .rightAs(AgentCommand.Response.Accepted)

      case fileWatch: FileWatch =>
        if !conf.subagentConf.scriptInjectionAllowed then
          IO.left(SignedInjectionNotAllowed)
        else
          fileWatchManager.update(fileWatch)
            .map(_.rightAs(AgentCommand.Response.Accepted))

      case item @ (_: AgentRef | _: Calendar | _: SubagentBundle |
                   _: WorkflowPathControl | _: WorkflowControl) =>
        persist(ItemAttachedToMe(item))
          .flatMapT: _ =>
            proceedWithItem(item)
          .rightAs(AgentCommand.Response.Accepted)

      case _ =>
        IO.left(Problem.pure(s"AgentCommand.AttachItem(${item.key}) for unknown InventoryItem"))

  private def attachSignedItem(signed: Signed[SignableItem]): IO[Checked[Response.Accepted]] =
    forDirector.signatureVerifier.verify(signed.signedString) match
      case Left(problem) =>
        logger.warn(s"${signed.value.key} could not be verified: $problem")
        IO.left(problem)
      case Right(signerIds) =>
        logger.info(Logger.SignatureVerified,
          s"Verified ${signed.value.key}, signed by ${signerIds.mkString(", ")}")
        persist: agentState =>
          signed.value match
            case workflow: Workflow =>
              agentState.idToWorkflow.get(workflow.id) match
                case None =>
                  // IMPORTANT: Check ZoneId to allow problem-free access after being persisted
                  workflow.timeZone.toZoneId.map: _ =>
                    SignedItemAttachedToMe(signed) :: Nil

                case Some(registeredWorkflow) =>
                  if workflow.withoutSource.reduceForAgent(agentPath) !=
                    registeredWorkflow.withoutSource
                  then
                    logger.warn(s"AttachSignedItem: Different duplicate ${workflow.id}:")
                    logger.warn(s"AttachSignedItem  ${workflow.withoutSource.asJson.toPrettyString}")
                    logger.warn(s"But registered is ${registeredWorkflow.withoutSource.asJson.toPrettyString}")
                    Left(Problem.pure(s"Different duplicate ${workflow.id}"))
                  else
                    Right(Nil)

            case _: JobResource =>
              Right:
                SignedItemAttachedToMe(signed) :: Nil

            case _ =>
              Left(Problem.pure:
                s"AgentCommand.AttachSignedItem(${signed.value.key}) for unknown SignableItem")
        .flatMapT: persisted =>
          persisted.keyedEvents.foldMap:
            case KeyedEvent(NoKey, SignedItemAttachedToMe(Signed(workflow: Workflow, _))) =>
              jobMotor.createJobEntries(persisted.aggregate.idToWorkflow(workflow.id))
            case _ => IO.unit
          .map(Right(_))
        .rightAs(AgentCommand.Response.Accepted)

  private def detachItem(itemKey: InventoryItemKey): IO[Checked[AgentCommand.Response]] =
    def persistItemDetachedIfExists =
      persist: agentState =>
        ifItemKeyExists(agentState):
          ItemDetached(itemKey, agentPath)
      .ifPersisted: persisted =>
        persisted.keyedEvents.foldMap:
          case KeyedEvent(NoKey, ItemDetached(WorkflowId.as(workflowId), _)) =>
            jobMotor.deleteEntriesForWorkflow(workflowId)
          case _ => IO.unit

    def ifItemKeyExists[E <: InventoryItemEvent](agentState: AgentState)(event: => E) =
      if !agentState.keyToItem.contains(itemKey) then
        logger.warn(s"DetachItem($itemKey) but item is unknown")
        Right(Nil)
      else
        Right((NoKey <-: event) :: Nil)

    itemKey match
      case path: OrderWatchPath =>
        fileWatchManager.remove(path)
          .rightAs(AgentCommand.Response.Accepted)

      case subagentId: SubagentId =>
        journal.persist: agentState =>
          ifItemKeyExists(agentState):
            ItemDetachingFromMe(subagentId)
        .ifPersisted: _ =>
          subagentKeeper.removeSubagent(subagentId)
            .handleError[Unit]: t =>
              logger.error(s"removeSubagent($subagentId) => $t")
            .startAndForget // May take a short time
            // SubagentKeeper will eventually emit ItemDetached event
        .rightAs(AgentCommand.Response.Accepted)

      case bundleId: SubagentBundleId =>
        persistItemDetachedIfExists.ifPersisted: _ =>
          subagentKeeper.removeSubagentBundle(bundleId)
        .rightAs(AgentCommand.Response.Accepted)

      case WorkflowId.as(workflowId) =>
        persistItemDetachedIfExists.ifPersisted: persisted =>
          persisted.originalAggregate.idToWorkflow.get(workflowId).fold(IO.unit): workflow =>
            subagentKeeper
              .stopJobs(workflow.keyToJob.keys, SIGKILL /*just in case*/)
              .handleError: t =>
                logger.error(s"SubagentKeeper.stopJobs: ${t.toStringWithCauses}", t)
        .rightAs(AgentCommand.Response.Accepted)

      case _ =>
        persistItemDetachedIfExists
          .rightAs(AgentCommand.Response.Accepted)
  end detachItem

  private def proceedWithItem(item: InventoryItem): IO[Checked[Unit]] =
    item match
      case agentRef: AgentRef =>
        //TODO val processLimitIncreased = previous
        //  .collect { case o: AgentRef => o.processLimit }
        //  .flatten
        //  .forall(previous => agentRef.processLimit.forall(previous < _))
        //if processLimitIncreased then
        jobMotor.tryStartProcessing.map(Right(_))

      case subagentItem: SubagentItem =>
        journal.aggregate.flatMap:
          _.idToSubagentItemState.get(subagentItem.id)
            .fold(IO.pure(Checked.unit)): subagentItemState =>
              subagentKeeper.proceedWithSubagent(subagentItemState)
                .catchIntoChecked
        .flatMapT: _ =>
          jobMotor.tryStartProcessing.map(Right(_))

      case subagentBundle: SubagentBundle =>
        subagentKeeper.addOrReplaceSubagentBundle(subagentBundle)
          .flatMapT: _ =>
            jobMotor.tryStartProcessing.map(Right(_))

      case workflowPathControl: WorkflowPathControl =>
        if !workflowPathControl.suspended then
          proceedWithOrders:
            agentState().orders.view // Slow !!!
              .filter(_.workflowPath == workflowPathControl.workflowPath)
              .toVector
          .map(Right(_))
        else
          IO.right(())

      case _ => IO.right(())

  @volatile private var changeSubagentAndClusterNodeAndProceedFiberStop = false
  private val changeSubagentAndClusterNodeAndProceedFiber =
    AsyncVariable(pureFiberIO(Checked.unit))

  private def changeSubagentAndClusterNodeThenProceed(event: ItemAttachedToMe): IO[Checked[Unit]] =
    // TODO Behaviour could be improved
    changeSubagentAndClusterNodeAndProceedFiber
      .update: fiber =>
        changeSubagentAndClusterNodeAndProceedFiberStop = true
        fiber.joinStd *>
          IO.defer:
            changeSubagentAndClusterNodeAndProceedFiberStop = false
            tryForeverChangeSubagentAndClusterNodeAndProceed(event)
              .startAndLogError
      .flatMap(_.joinStd.timeoutTo(10.s/*???*/, IO.right(()) /*respond the command*/))

  private def tryForeverChangeSubagentAndClusterNodeAndProceed(event: ItemAttachedToMe)
  : IO[Checked[Unit]] =
    IO.defer:
      import event.item
      val label = s"${event.getClass.simpleScalaName}(${item.key})"
      val since = now
      val delays = Iterator(1.s, 3.s, 6.s, 10.s).continueWithLast
      val sym = new BlockingSymbol
      ().tailRecM: _ =>
        changeSubagentAndClusterNode(event)
          .flatMapT:_ =>
            proceedWithItem(item)
          .uncancelable // Only the loop should be cancelable, but not the inner operations
          .flatMap:
            case Left(problem @ PassiveClusterNodeUrlChangeableOnlyWhenNotCoupledProblem) =>
              sym.onWarn()
              logger.warn(s"$sym $label => $problem — trying since ${since.elapsed.pretty} ...")
              IO.sleep(delays.next()).as(Left(())/*repeat*/)

            case checked =>
              IO:
                if sym.used then checked match
                  case Left(problem) => logger.error(s"🔥 $label: $problem")
                  case Right(()) => logger.info(s"🟢 $label: Cluster setting has been changed")
                Right(checked)

  /** Emits the event, and ClusterSettingUpdated if needed, in separate transaction. */
  private def changeSubagentAndClusterNode(event: ItemAttachedToMe): IO[Checked[Unit]] =
    logger.debugIO:
      journal.aggregate.flatMap: agentState =>
        if !agentState.isDedicated then
          journal.persist(event).rightAs(())
        else
          IO.pure(agentState.applyKeyedEvent(event)).flatMapT: nextAgentState =>
            demandedClusterNodeUris(nextAgentState) match
              case None =>
                journal.persist(event).rightAs(())

              case Some(idToUri) =>
                agentState.clusterState.match
                  case ClusterState.Empty =>
                    Some(NodeId.primary)
                  case clusterState: ClusterState.HasNodes =>
                    (clusterState.setting.idToUri != idToUri) ? clusterState.activeId
                .match
                  case None =>
                    journal.persist(event).rightAs(())
                  case Some(activeNodeId) =>
                    workingClusterNode.appointNodes(idToUri, activeNodeId, extraEvent = Some(event))

  /** Returns Some when AgentRef and the Director's SubagentIds are available. */
  private def demandedClusterNodeUris(agentState: AgentState): Option[Map[NodeId, Uri]] =
    for
      agentRef <- agentState.keyToItem(AgentRef).get(agentState.meta.agentPath)
      if agentRef.directors.length == 2
      subagentItems <- agentRef.directors.traverse(agentState.keyToItem(SubagentItem).get)
      if subagentItems.length == 2
    yield
      Map(
        NodeId.primary -> subagentItems(0).uri,
        NodeId.backup -> subagentItems(1).uri)

  def resetAllSubagents: IO[Unit] =
    journal.aggregate.flatMap: agentState =>
      subagentKeeper.resetAllSubagents(except = agentState.meta.directors.toSet)

  private def persist[E <: Event](keyedEvent: KeyedEvent[E]): IO[Checked[Persisted[AgentState, E]]] =
    persist(EventCalc.pure(keyedEvent))

  private def persist[E <: Event](toEvents: AgentState => Checked[IterableOnce[KeyedEvent[E]]])
  : IO[Checked[Persisted[AgentState, E]]] =
    persist(EventCalc.checked[AgentState, E, TimeCtx](toEvents(_)))

  private def persist[E <: Event](eventCalc: EventCalc[AgentState, E, TimeCtx])
  : IO[Checked[Persisted[AgentState, E]]] =
    journal.persist(eventCalc).flatTapT(onPersisted)

  private def onPersisted(persisted: Persisted[AgentState, Event]): IO[Right[Problem, Unit]] =
    onPersisted2(persisted).map(Right(_))

  private def onPersisted2(persisted: Persisted[AgentState, Event]): IO[Unit] =
    persisted.keyedEvents.toVector.flatTraverse:
      case KeyedEvent(orderId: OrderId, event: OrderForked) =>
        IO.pure(orderId +: event.children.map(_.orderId))

      case KeyedEvent(orderId: OrderId, OrderKillingMarked(Some(kill))) =>
        persisted.aggregate.idToOrder.get(orderId).fold(IO.unit): order =>
          maybeKillOrder(order, kill)
        .as(Vector(orderId))

      case KeyedEvent(orderId: OrderId, OrderDetached) =>
        orderToEntry.remove(orderId).flatMapSome:
          _.stop
        .as(Vector(orderId))

      case KeyedEvent(orderId: OrderId, _) =>
        IO.pure(Vector(orderId))

      case _ =>
        IO.pure(Vector.empty)
    .flatMap: touchedOrderIds =>
      proceedWithOrders:
        touchedOrderIds.distinct
          .flatMap(persisted.aggregate.idToOrder.get)

  private def onSubagentEvents(orderId: OrderId)
    (events: Iterable[OrderStarted | OrderProcessingStarted | OrderProcessed])
  : IO[Unit] =
    events.foldMap:
      case OrderStarted => IO.unit
      case _: OrderProcessingStarted => maybeKillOrder(orderId)
      case _: OrderProcessed => jobMotor.onOrderProcessed(orderId)
    .productR:
      proceedWithOrder(orderId)

  private def proceedWithOrder(orderId: OrderId): IO[Unit] =
    withCurrentOrder(orderId): order =>
      proceedWithOrders(order :: Nil)

  private def proceedWithOrders(orders: Seq[Order[Order.State]]): IO[Unit] =
    whenDeferred(orders.nonEmpty && !isStopping):
      clock.lockIO: now =>
        val (immediateOrderIds, delayOrderIds) =
          orders.map: order =>
            order.id -> order.maybeDelayedUntil.getOrElse(Timestamp.Epoch)
          .partition(_._2 <= now)
        delayOrderIds.foldMap: (orderId, delayUntil) =>
          orderToEntry.getOrElseUpdate(orderId, IO.pure(OrderEntry(orderId))).flatMap: entry =>
            entry.schedule(delayUntil):
              proceedWithOrder(orderId) // Try again after delay
        .as(immediateOrderIds.map(_._1))
      .flatMap: (orderIds: Seq[OrderId]/*IntelliJ*/) =>
        // persist calls recursively this proceedWithOrders !!! TODO use Queue and Stream
        persist: agentState =>
          Right:
            orderIds.flatMap: orderId =>
              OrderEventSource(agentState)(using InstructionExecutorService(clock))
                .nextEvents(orderId)
        .map(_.orThrow) // TODO throws
        .flatMap: persisted =>
          orderIds.foldMap: orderId =>
            persisted.aggregate.idToOrder.get(orderId).fold(IO.unit): order =>
              IO.whenA(persisted.aggregate.isOrderProcessable(orderId)):
                jobMotor.onOrderIsProcessable(order)

  def shutdown(cmd: AgentCommand.ShutDown): IO[Unit] =
    _shutdown.set(Some(cmd)) *>
      stop

  def inhibitShutdownLocalSubagent: IO[Unit] =
    _inhibitShutdownLocalSubagent.set(true)

  private def maybeKillOrder(orderId: OrderId): IO[Unit] =
    withCurrentOrder(orderId): order =>
      order.ifState[Order.Processing].fold(IO.unit): order =>
        order.mark match
          case Some(OrderMark.Cancelling(CancellationMode.FreshOrStarted(Some(kill)))) =>
            maybeKillOrder(order, kill)

          case Some(OrderMark.Suspending(SuspensionMode(_, Some(mode)))) =>
            maybeKillOrder(order, mode)

          case _ => IO.unit

  private def maybeKillOrder(order: Order[Order.State], kill: CancellationMode.Kill): IO[Unit] =
    order.ifState[Order.Processing].fold(IO.unit): order =>
      IO.whenA(kill.workflowPosition.forall(_ == order.workflowPosition)):
        subagentKeeper.killProcess(
          order.id,
          if kill.immediately then SIGKILL else SIGTERM)

  private def withCurrentOrder[A](orderId: OrderId)(body: Order[Order.State] => IO[Unit]): IO[Unit] =
    journal.aggregate.flatMap: agentState =>
      agentState.idToOrder.get(orderId).foldMap:
        body

  private def agentState(): AgentState =
    journal.unsafeAggregate()

  override def toString = "AgentMotor"


object AgentMotor:
  private val logger = Logger[this.type]

  /** AgentMotor with SubagentKeeper (including local Subagent) and FileWatchManager. */
  def resource(
    failedOverSubagentId: Option[SubagentId],
    forDirector: Subagent.ForDirector,
    workingClusterNode: WorkingClusterNode[AgentState],
    conf: AgentConfiguration,
    actorSystem: ActorSystem)
  : ResourceIO[AgentMotor] =
    Resource.suspend:
      import forDirector.subagent as localSubagent
      val journal = workingClusterNode.journal
      journal.aggregate.map: agentState =>
        val meta = agentState.meta
        val subagentId: SubagentId =
          if meta.directors.isEmpty then throw new IllegalStateException(
            "Missing definition of Subagents in AgentMetaState")
          else if !conf.clusterConf.isBackup then
            meta.directors.head
          else if meta.directors.sizeIs < 2 then throw new IllegalStateException(
            "Missing definition of backup Subagent in AgentMetaState")
          else
            meta.directors(1)
        (meta.controllerId, meta.agentPath, subagentId)
      .flatMap: (controllerId, ownAgentPath, localSubagentId) =>
        // Automatically add Controller's UserId to the list of users allowed to release events,
        // to avoid deletion of journal files due to an empty list, before controller has read the events.
        // The controller has to send ReleaseEvents commands to release obsolete journal files.
        journal.persist: agentState =>
          val userId = agentState.meta.controllerId.toUserId
          !agentState.journalState.userIdToReleasedEventId.contains(userId) ?
            JournalEventsReleased(userId, EventId.BeforeFirst)
        .map(_.orThrow)
        .map: (_: Persisted[AgentState, Event]) =>
          for
            fileWatchManager <- FileWatchManager.resource(ownAgentPath, journal, conf.config)
            given IORuntime <- Resource.eval(environment[IORuntime])
            clock <- Resource.eval(environment[AlarmClock])
            subagentKeeper <- SubagentKeeper.resource(
              localSubagentId, localSubagent, ownAgentPath, controllerId, failedOverSubagentId,
              journal, conf.directorConf, actorSystem)
            dispatcher <- Dispatcher.parallel[IO]
            agentMotor <- Service.resource:
              new AgentMotor(
                failedOverSubagentId, controllerId, ownAgentPath, localSubagentId,
                subagentKeeper, forDirector, fileWatchManager, workingClusterNode, conf,
              )(using clock, dispatcher)
          yield
            agentMotor


  private final class OrderEntry(orderId: OrderId):
    private val cancelScheduleRef = Ref.unsafe[IO, IO[Unit]](IO.unit)

    def stop: IO[Unit] =
      cancelScheduleRef.getAndSet(IO.unit).flatten

    def schedule(timestamp: Timestamp)(io: IO[Unit])
      (using clock: AlarmClock, dispatcher: Dispatcher[IO])
    : IO[Unit] =
      clock.scheduleIOAt(timestamp, label = orderId.toString):
        io
      .flatMap: cancel =>
        cancelScheduleRef.getAndSet(cancel)
          .flatten/*cancel previous schedule*/
