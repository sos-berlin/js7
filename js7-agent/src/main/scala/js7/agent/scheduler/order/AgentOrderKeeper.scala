package js7.agent.scheduler.order

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.flatMap.*
import cats.syntax.parallel.*
import com.softwaremill.tagging.@@
import io.circe.syntax.EncoderOps
import java.time.ZoneId
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachItem, AttachOrder, AttachSignedItem, DetachItem, DetachOrder, MarkOrder, OrderCommand, ReleaseEvents, Response}
import js7.agent.data.event.AgentEvent.{AgentReady, AgentShutDown}
import js7.agent.scheduler.order.AgentOrderKeeper.*
import js7.base.catsutils.CatsEffectExtensions.{joinStd, left, materializeIntoChecked, right}
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.crypt.Signed
import js7.base.generic.Completed
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.{BlockingSymbol, CorrelId, Logger}
import js7.base.monixlike.MonixLikeExtensions.{deferFuture, foreach, materialize}
import js7.base.monixutils.AsyncVariable
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem, WrappedException}
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.JavaTime.*
import js7.base.time.ScalaTime.*
import js7.base.time.{AdmissionTimeScheme, AlarmClock, TimeInterval}
import js7.base.utils.CatsUtils.pureFiberIO
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.Collections.implicits.InsertableMutableMap
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{DuplicateKeyException, SetOnce}
import js7.cluster.WorkingClusterNode
import js7.common.pekkoutils.Pekkos.{encodeAsActorName, uniqueActorName}
import js7.common.pekkoutils.SupervisorStrategies
import js7.common.system.PlatformInfos.currentPlatformInfo
import js7.common.system.startup.ServiceMain
import js7.common.utils.Exceptions.wrapException
import js7.core.problems.ReverseReleaseEventsProblem
import js7.data.Problems.PassiveClusterNodeUrlChangeableOnlyWhenNotCoupledProblem
import js7.data.agent.AgentRef
import js7.data.agent.Problems.{AgentDuplicateOrder, AgentIsShuttingDown}
import js7.data.calendar.Calendar
import js7.data.event.JournalEvent.JournalEventsReleased
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{<-:, Event, EventCalc, EventId, JournalState, KeyedEvent, Stamped}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.{ExecuteAdmissionTimeSwitch, InstructionExecutorService}
import js7.data.item.BasicItemEvent.{ItemAttachedToMe, ItemDetached, ItemDetachingFromMe, SignedItemAttachedToMe}
import js7.data.item.{InventoryItem, SignableItem, UnsignedItem}
import js7.data.job.{JobKey, JobResource}
import js7.data.order.Order.InapplicableOrderEventProblem
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAttachedToAgent, OrderCoreEvent, OrderDetached, OrderProcessed}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.plan.PlanFinishedEvent
import js7.data.state.OrderEventHandler
import js7.data.state.OrderEventHandler.FollowUp
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentId, SubagentItem}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowId, WorkflowPathControl}
import js7.journal.JournalingActor
import js7.launcher.configuration.Problems.SignedInjectionNotAllowed
import js7.subagent.Subagent
import js7.subagent.director.SubagentKeeper
import org.apache.pekko.actor.{ActorRef, Stash, SupervisorStrategy, Terminated}
import org.apache.pekko.pattern.{ask, pipe}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.language.unsafeNulls
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
 * Keeper of one Controller's orders.
 *
 * @author Joacim Zschimmer
 */
final class AgentOrderKeeper(
  forDirector: Subagent.ForDirector,
  workingClusterNode: WorkingClusterNode[AgentState],
  failedOverSubagentId: Option[SubagentId],
  recoveredAgentState : AgentState,
  changeSubagentAndClusterNode: ItemAttachedToMe => IO[Checked[Unit]],
  shutDownOnce: SetOnce[AgentCommand.ShutDown],
  private implicit val clock: AlarmClock,
  conf: AgentConfiguration)
  (implicit protected val ioRuntime: IORuntime)
extends JournalingActor[AgentState, Event], Stash:

  import conf.implicitPekkoAskTimeout
  import context.{actorOf, watch}
  import forDirector.subagent as localSubagent

  private given ExecutionContext = ioRuntime.compute

  private val journal = workingClusterNode.journal
  private val (ownAgentPath, localSubagentId, controllerId) =
    val meta = agentState().meta
    val subagentId: SubagentId =
      if meta.directors.isEmpty then throw new IllegalStateException(
        "Missing definition of Subagents in AgentMetaState")
      else if !conf.clusterConf.isBackup then
        meta.directors.head
      else if meta.directors.sizeIs < 2 then throw new IllegalStateException(
        "Missing definition of backup Subagent in AgentMetaState")
      else
        meta.directors(1)
    (meta.agentPath, subagentId, meta.controllerId)
  private implicit val instructionExecutorService: InstructionExecutorService =
    new InstructionExecutorService(clock)

  override val supervisorStrategy: SupervisorStrategy =
    SupervisorStrategies.escalate

  protected val journalActor = workingClusterNode.journalActor

  protected def journalConf = conf.journalConf

  private var journalState = JournalState.empty
  private var agentProcessCount = 0
  private val jobRegister = mutable.Map.empty[JobKey, JobEntry]
  private val workflowRegister = new WorkflowRegister(ownAgentPath)
  private val fileWatchManager = new FileWatchManager(ownAgentPath, journal, conf.config)
  private val orderRegister = new OrderRegister
  private var switchingOver = false

  private object shutdown:
    private var shutDownCommand: Option[AgentCommand.ShutDown] = None
    private var terminatingOrders = false
    private var terminatingJobs = false
    private var terminatingJournal = false
    private var agentShutDownEmitted = false
    val since = SetOnce[Deadline]

    def shuttingDown = shutDownCommand.isDefined

    def start(cmd: AgentCommand.ShutDown): Unit =
      if !shuttingDown then
        since := now
        shutDownCommand = Some(cmd)
        fileWatchManager.stop.unsafeRunAndForget()
        if cmd.isFailover then
          (journal.kill *> IO(context.stop(self)))
            .unsafeRunAndForget()
        if cmd.suppressSnapshot then
          journal.suppressSnapshotWhenStopping()
        if cmd.isFailOrSwitchover then
          //workingClusterNode.journalAllocated.release.unsafeRunAndForget()
          context.stop(self)
        continue()

    def onStillTerminating(): Unit =
      logger.info(s"🟠 Still terminating, waiting for ${orderRegister.size} orders" +
        s", ${jobRegister.size} jobs")

    def continue(): Unit =
      for shutDown <- shutDownCommand do
        logger.trace(s"termination.continue: ${orderRegister.size} orders, " +
          jobRegister.size + " jobs")
        if !terminatingOrders then
          terminatingOrders = true
          for o <- orderRegister.values if !o.isDetaching do
            o.actor ! OrderActor.Input.Terminate(shutDown.processSignal/*only local Subagent*/)
        if orderRegister.isEmpty then
          if !terminatingJobs then
            terminatingJobs = true
            subagentKeeper.stop
              .handleError: t =>
                logger.error(s"subagentKeeper.stop =>${t.toStringWithCauses}", t)
              .map(_ => self ! Internal.JobDriverStopped)
              .unsafeRunAndForget()
          if jobRegister.isEmpty && !terminatingJournal then
            IO.whenA(!shutDown.isFailOrSwitchover):
              workingClusterNode.shutDownThisNode.flatMap:
                case Right(Completed) => IO.unit
                case Left(problem) =>
                  IO(logger.warn(s"workingClusterNode.shutDownThisNode => $problem"))
            .map: _ =>
              self ! Internal.AgentShutdown
            .unsafeRunAndForget()
          end if

    def finallyShutdown(): Unit =
      if agentShutDownEmitted then
        logger.debug("❓ Duplicate finallyShutdown")
      else
        persist(AgentShutDown): (_, _) =>
          agentShutDownEmitted = true
          terminatingJournal = true
          //workingClusterNode.journalAllocated.release.unsafeRunAndForget()
          context.stop(self)


  import shutdown.shuttingDown

  private val subagentKeeper =
    new SubagentKeeper(
      localSubagentId, localSubagent, ownAgentPath, controllerId, failedOverSubagentId,
      journal, conf.directorConf, context.system)

  journal.untilStopped.productR(IO(self ! Internal.JournalStopped)).unsafeRunAndForget()
  self ! Internal.Recover(recoveredAgentState)
  // Do not use recovered_ after here to allow release of the big object

  override def postStop(): Unit =
    // TODO Use Resource (like the Subagent starter)
    // TODO Blocking!
    try
      subagentKeeper.stop
        .uncancelable // TOOD Deadlock possible, probably due to stopped Journal. We time-out:
        .start.flatMap(_.joinStd).timeout(3.s)
        .logWhenItTakesLonger("subagentKeeper.stop")
        .await(99.s)
    catch case NonFatal(t) => logger.error(s"subagentKeeper.stop => ${t.toStringWithCauses}")

    fileWatchManager.stop.unsafeRunAndForget()
    super.postStop()
    logger.debug("Stopped" + shutdown.since.toOption.fold("")(o => s" (terminated in ${o.elapsed.pretty})"))

  def receive: Receive =
    case Internal.Recover(recoveredAgentState) =>
      journalState = recoveredAgentState.journalState

      become("Recovering")(recovering(recoveredAgentState))
      unstashAll()

      journal.aggregate
        .flatMap: agentState =>
          subagentKeeper.recoverSubagents(agentState.idToSubagentItemState.values.toVector)
        .flatMapT(_ =>
          subagentKeeper.recoverSubagentBundles(
            recoveredAgentState.pathToUnsignedSimple(SubagentBundle).values.toVector))
        .map(_.orThrow)
        .materialize
        .foreach { tried =>
          self.forward(Internal.SubagentKeeperInitialized(recoveredAgentState, tried))
        }

    case _ => stash()

  private def recovering(recoveredState: AgentState): Receive =
    var remainingOrders = recoveredState.idToOrder.size

    def continue() =
      if remainingOrders == 0 then
        subagentKeeper.start
          .logWhenItTakesLonger("subagentKeeper.start")
          .awaitInfinite

        if !journalState.userIdToReleasedEventId.contains(controllerId.toUserId) then
          // Automatically add Controller's UserId to list of users allowed to release events,
          // to avoid deletion of journal files due to an empty list, before controller has read the events.
          // The controller has to send ReleaseEvents commands to release obsolete journal files.
          persist(JournalEventsReleased(controllerId.toUserId, EventId.BeforeFirst)):
            case (Stamped(_,_, _ <-: event), journaledState) =>
              journalState = journalState.applyEvent(event)/*:Right*/.value

        fileWatchManager.start
          .map(_.orThrow)  // How to handle a failure, due to missing environment variable ???
          .unsafeRunAndForget()

        // TODO AgentReady should be the first event ?
        persist(
          AgentReady(
            ZoneId.systemDefault.getId,
            totalRunningTime = journal.totalRunningTime,
            Some(currentPlatformInfo()))
        ) { (_, _) =>
          workingClusterNode.afterJournalingStarted
            .materializeIntoChecked
            .as:
              Internal.OrdersRecovered(recoveredState)
            .unsafeToFuture()
            .pipeTo(self)
        }

    val receive: Receive =
      case Internal.SubagentKeeperInitialized(state, tried) =>
        for t <- tried.ifFailed do throw WrappedException(t)

        for workflow <- state.idToWorkflow.values do
          wrapException(s"Error while recovering ${workflow.path}"):
            workflowRegister.recover(workflow)
            val timeZone = ZoneId.of(workflow.timeZone.string) // throws on unknown time zone !!!
            createJobEntries(workflow, timeZone)

        for order <- state.idToOrder.values do
          wrapException(s"Error while recovering ${order.id}"):
            //val order = workflowRegister.reuseMemory(recoveredOrder)
            val actor = newOrderActor(order.id)
            orderRegister.recover(order.id, actor)
            actor ! OrderActor.Input.Recover(order)

        continue()

      case OrderActor.Output.RecoveryFinished =>
        remainingOrders -= 1
        continue()

      case Internal.OrdersRecovered(state) =>
        for order <- state.idToOrder.values.view.flatMap(_.ifState[Order.Processing]) do
          for jobKey <- state.jobKey(order.workflowPosition).toOption do
            jobRegister(jobKey).recoverProcessingOrder(order)
            agentProcessCount += 1

        // proceedWithOrder before subagentKeeper.start because continued Orders (still
        // processing at remote Subagent) will emit events and change idToOrder asynchronously!
        // But not before SubagentKeeper has been started (when Subagents are coupled).
        for order <- state.idToOrder.values do
          proceedWithOrder(order)

        logger.info(ServiceMain.readyMessageWithLine(s"$ownAgentPath is ready"))
        become("ready")(ready)
        unstashAll()

      case _: AgentCommand.ShutDown =>
        logger.info("ShutDown command terminates Agent while recovering")
        context.stop(self)
        sender() ! AgentCommand.Response.Accepted

      case _ =>
        stash()
    receive

  private def ready: Receive =
    case Input.ExternalCommand(cmd, correlId, response) =>
      response.completeWith(
        correlId.bind {
          processCommand(cmd)
        })

    case cmd: AgentCommand.ShutDown =>
      if cmd.isSwitchover then
        switchOver(cmd) pipeTo sender()
      else
        shutdown.start(cmd)
        sender() ! AgentCommand.Response.Accepted

    case OrderActor.Output.OrderChanged(orderId, correlId, previousOrderOrNull, events) =>
      correlId.bind[Unit]:
        if !shuttingDown then
          // previousOrderOrNull is null only for OrderAttachedToAgent event
          var order = previousOrderOrNull
          var myJobEntry: JobEntry = null
          for event <- events do
            event match
              case event: OrderAttachedToAgent =>
                order = Order.fromOrderAttached(orderId, event)

              case event: OrderProcessed =>
                (for
                  jobKey <- agentState().jobKey(previousOrderOrNull.workflowPosition)
                  jobEntry <- jobRegister.checked(jobKey)
                yield jobEntry)
                match
                  case Left(problem) =>
                    logger.error(s"OrderActor.Output.OrderChanged($orderId) => $problem")

                  case Right(jobEntry) =>
                    jobEntry.processCount -= 1
                    agentProcessCount -= 1
                    myJobEntry = jobEntry
                order = order.applyEvent(event).orThrow

              case event: OrderCoreEvent =>
                order = order.applyEvent(event).orThrow

              case _ =>
            handleOrderEvent(order, event)
          if myJobEntry != null then
            tryStartProcessing(myJobEntry)
            tryStartProcessing()
          if !events.lastOption.contains(OrderDetached) then
            proceedWithOrder(order)

    case Internal.Due(orderId) if orderRegister.contains(orderId) =>
      if !shuttingDown then
        proceedWithOrder(orderId)

    case Internal.JobDue(jobKey) =>
      if !shuttingDown then
        for jobEntry <- jobRegister.get(jobKey) do
          tryStartProcessing(jobEntry)

    case Internal.TryStartProcessing =>
      tryStartProcessing()

    case Input.ResetAllSubagents =>
      subagentKeeper
        .resetAllSubagents(except = agentState().meta.directors.toSet)
        .void.unsafeToFuture().pipeTo(sender())

  private def processCommand(cmd: AgentCommand): Future[Checked[Response]] = cmd match
    case cmd: OrderCommand => processOrderCommand(cmd)

    case AttachItem(item) =>
      attachUnsignedItem(item)

    case AttachSignedItem(signed: Signed[SignableItem]) =>
      attachSignedItem(signed)

    case DetachItem(itemKey) if itemKey.isAssignableToAgent =>
      if !agentState().keyToItem.contains(itemKey) then
        logger.warn(s"DetachItem($itemKey) but item is unknown")
        Future.successful(Right(AgentCommand.Response.Accepted))
      else
        itemKey match
          case path: OrderWatchPath =>
            fileWatchManager.remove(path)
              .rightAs(AgentCommand.Response.Accepted)
              .unsafeToFuture()

          case subagentId: SubagentId =>
            journal
              .persist(NoKey <-: ItemDetachingFromMe(subagentId))
              .flatMapT(_ => subagentKeeper
                .startRemoveSubagent(subagentId)
                // SubagentKeeper will emit ItemDetached event
                .map(Right(_)))
              .as(Right(AgentCommand.Response.Accepted))
              .unsafeToFuture()

          case bundleId: SubagentBundleId =>
            journal
              .persist(NoKey <-: ItemDetached(bundleId, ownAgentPath))
              .flatMapT(_ => subagentKeeper
                .removeSubagentBundle(bundleId)
                .as(Right(AgentCommand.Response.Accepted)))
              .unsafeToFuture()

          case WorkflowId.as(workflowId) =>
            val maybeWorkflow = agentState().idToWorkflow.get(workflowId)
            persist(ItemDetached(itemKey, ownAgentPath)) { (stampedEvent, journaledState) =>
              for workflow <- maybeWorkflow do
                subagentKeeper
                  .stopJobs(workflow.keyToJob.keys, SIGKILL/*just in case*/)
                  .handleError: t =>
                    logger.error(s"SubagentKeeper.stopJobs: ${t.toStringWithCauses}", t)
                  .unsafeRunAndForget()
              Right(AgentCommand.Response.Accepted)
            }

          case _ =>
            persist(ItemDetached(itemKey, ownAgentPath)) { (stampedEvent, journaledState) =>
              Right(AgentCommand.Response.Accepted)
            }

    case AgentCommand.ResetSubagent(subagentId, force) =>
      subagentKeeper.startResetSubagent(subagentId, force)
        .rightAs(AgentCommand.Response.Accepted)
        .unsafeToFuture()

    case AgentCommand.ClusterSwitchOver =>
      switchOver(cmd)

    case AgentCommand.TakeSnapshot =>
      journal.takeSnapshot
        .as(Right(AgentCommand.Response.Accepted))
        .unsafeToFuture()

    case _ => Future.successful(Left(Problem(s"Unknown command: ${cmd.getClass.simpleScalaName}")))  // Should not happen

  private def attachUnsignedItem(item: UnsignedItem): Future[Checked[Response.Accepted]] =
    item match
      case agentRef: AgentRef =>
        if agentRef.path != ownAgentPath then
          Future.successful(Left(Problem(s"Alien AgentRef(${agentRef.path})")))
        else
          changeSubagentAndClusterNodeThenProceed(ItemAttachedToMe(agentRef))
            .rightAs(AgentCommand.Response.Accepted)
            .unsafeToFuture()

      case fileWatch: FileWatch =>
        if !conf.subagentConf.scriptInjectionAllowed then
          Future.successful(Left(SignedInjectionNotAllowed))
        else
          fileWatchManager.update(fileWatch)
            .map(_.rightAs(AgentCommand.Response.Accepted))
            .unsafeToFuture()

      case item: SubagentItem =>
        changeSubagentAndClusterNodeThenProceed(ItemAttachedToMe(item))
          .rightAs(AgentCommand.Response.Accepted)
          .unsafeToFuture()

      case item @ (_: AgentRef | _: Calendar | _: SubagentBundle |
                   _: WorkflowPathControl | _: WorkflowControl) =>
        //val previousItem = agentState().keyToItem.get(item.key)
        persist(ItemAttachedToMe(item)) { (stampedEvent, journaledState) =>
          proceedWithItem(/*previousItem,*/ item).unsafeToFuture()
        }.flatten
          .rightAs(AgentCommand.Response.Accepted)

      case _ =>
        Future.successful(Left(Problem.pure(s"AgentCommand.AttachItem(${item.key}) for unknown InventoryItem")))

  @volatile private var changeSubagentAndClusterNodeAndProceedFiberStop = false
  private val changeSubagentAndClusterNodeAndProceedFiber =
    AsyncVariable(pureFiberIO(Checked.unit))

  private def changeSubagentAndClusterNodeThenProceed(event: ItemAttachedToMe): IO[Checked[Unit]] =
    changeSubagentAndClusterNodeAndProceedFiber
      .update: fiber =>
        changeSubagentAndClusterNodeAndProceedFiberStop = true
        fiber.joinStd *>
          IO.defer:
            changeSubagentAndClusterNodeAndProceedFiberStop = false
            tryForeverChangeSubagentAndClusterNodeAndProceed(event)
              .start
      .flatMap(_.joinStd.timeoutTo(10.s /*???*/ , IO.right(()) /*respond the command*/))

  private def tryForeverChangeSubagentAndClusterNodeAndProceed(event: ItemAttachedToMe)
  : IO[Checked[Unit]] =
    IO.defer:
      import event.item
      val label = s"${event.getClass.simpleScalaName}(${item.key})"
      val since = now
      val delays = Iterator(1.s, 3.s, 10.s).continueWithLast
      val sym = new BlockingSymbol
      ().tailRecM { _ =>
        changeSubagentAndClusterNode(event)
          .flatMapT(_ => proceedWithItem(item))
          .uncancelable // Only the loop should be cancelable, but not the inner operations
          .flatMap:
            case Left(problem @ PassiveClusterNodeUrlChangeableOnlyWhenNotCoupledProblem) =>
              sym.onWarn()
              logger.warn(
                s"$sym $label => $problem — trying since ${since.elapsed.pretty} ...")
              IO.sleep(delays.next()).as(Left(())/*repeat*/)

            case checked =>
              if sym.used then checked match
                case Left(problem) => logger.error(s"🔥 $label => $problem")
                case Right(()) => logger.info(s"🟢 $label => Cluster setting has been changed")
              IO.right(checked)
      }

  private def attachSignedItem(signed: Signed[SignableItem]): Future[Checked[Response.Accepted]] =
    forDirector.signatureVerifier.verify(signed.signedString) match
      case Left(problem) =>
        logger.warn(s"${signed.value.key} could not be verified: $problem")
        Future.successful(Left(problem))
      case Right(signerIds) =>
        logger.info(Logger.SignatureVerified,
          s"Verified ${signed.value.key}, signed by ${signerIds.mkString(", ")}")

        signed.value match
          case workflow: Workflow =>
            workflowRegister.get(workflow.id) match
              case None =>
                workflow.timeZone.toZoneId match
                  case Left(problem) => Future.successful(Left(problem))
                  case Right(zoneId) =>
                    persist(SignedItemAttachedToMe(signed)) { (stampedEvent, journaledState) =>
                      val reducedWorkflow = journaledState.idToWorkflow(workflow.id)
                      workflowRegister.handleEvent(stampedEvent.value, reducedWorkflow)
                      createJobEntries(reducedWorkflow, zoneId)
                      Right(AgentCommand.Response.Accepted)
                    }

              case Some(registeredWorkflow) =>
                Future.successful:
                  if workflow.withoutSource.reduceForAgent(ownAgentPath) != registeredWorkflow.withoutSource then
                    logger.warn(s"AttachSignedItem: Different duplicate ${workflow.id}:")
                    logger.warn(s"AttachSignedItem  ${workflow.withoutSource.asJson.toPrettyString}")
                    logger.warn(s"But registered is ${registeredWorkflow.withoutSource.asJson.toPrettyString}")
                    Left(Problem.pure(s"Different duplicate ${workflow.id}"))
                  else
                    Right(AgentCommand.Response.Accepted)

          case _: JobResource =>
            persistKeyedEvent(SignedItemAttachedToMe(signed)) { (stampedEvent, journaledState) =>
              Right(AgentCommand.Response.Accepted)
            }

          case _ =>
            Future.successful(Left(Problem.pure(s"AgentCommand.AttachSignedItem(${signed.value.key}) for unknown SignableItem")))

  private def proceedWithItem(/*previous: Option[InventoryItem],*/ item: InventoryItem)
  : IO[Checked[Unit]] =
    item match
      case agentRef: AgentRef =>
        //val processLimitIncreased = previous
        //  .collect { case o: AgentRef => o.processLimit }
        //  .flatten
        //  .forall(previous => agentRef.processLimit.forall(previous < _))
        //if processLimitIncreased then
        self ! Internal.TryStartProcessing
        IO.right(())

      case subagentItem: SubagentItem =>
        journal.aggregate.flatMap(_
          .idToSubagentItemState.get(subagentItem.id)
          .fold(IO.pure(Checked.unit))(subagentItemState => subagentKeeper
            .proceedWithSubagent(subagentItemState)
            .materializeIntoChecked))

      case subagentBundle: SubagentBundle =>
        subagentKeeper.addOrReplaceSubagentBundle(subagentBundle)

      case workflowPathControl: WorkflowPathControl =>
        if !workflowPathControl.suspended then
          // Slow !!!
          for order <- agentState().orders
               if order.workflowPath == workflowPathControl.workflowPath do
            proceedWithOrder(order)
        IO.right(())

      case _ => IO.right(())

  private def processOrderCommand(cmd: OrderCommand): Future[Checked[Response]] = cmd match
    case AttachOrder(order) =>
      if shuttingDown then
        Future.successful(Left(AgentIsShuttingDown))
      else
        order.attached match
          case Left(problem) => Future.successful(Left(problem))
          case Right(agentPath) =>
            if agentPath != ownAgentPath then
              Future.successful(Left(Problem(s"Wrong $agentPath")))
            else
              workflowRegister.get(order.workflowId) match
                case None =>
                  Future.successful(Left(Problem.pure(s"Unknown ${order.workflowId}")))
                case Some(workflow) =>
                  if !workflow.isDefinedAt(order.position) then
                    Future.successful(Left(Problem.pure(s"Unknown Position ${order.workflowPosition}")))
                  else if orderRegister.contains(order.id) then
                    Future.successful(Left(AgentDuplicateOrder(order.id)))
                  else if shuttingDown then
                    Future.successful(Left(AgentIsShuttingDown))
                  else
                    attachOrder(/*workflowRegister.reuseMemory*/(order))
                      .map((_: Completed) => Right(Response.Accepted))

    case DetachOrder(orderId) =>
      if shuttingDown then
        Future.successful(AgentIsShuttingDown)
      else
        orderRegister.get(orderId) match
          case Some(orderEntry) =>
            // TODO Antwort erst nach OrderDetached _und_ Terminated senden, wenn Actor aus orderRegister entfernt worden ist
            // Bei langsamem Agenten, schnellem Controller-Wiederanlauf kann DetachOrder doppelt kommen, während OrderActor sich noch beendet.
            agentState().idToOrder.checked(orderId).flatMap(_.detaching) match
              case Left(problem) => Future.successful(Left(problem))
              case Right(_) =>
                val promise = Promise[Unit]()
                orderEntry.detachResponses ::= promise
                (orderEntry.actor ? OrderActor.Command.HandleEvents(OrderDetached :: Nil, CorrelId.current))
                  .mapTo[Checked[Completed]]
                  .onComplete:
                    case Failure(t) => promise.tryFailure(t)
                    case Success(Left(problem)) => promise.tryFailure(problem.throwable)
                    case Success(Right(Completed)) =>
                      // Ignore this and instead await OrderActor termination and removal from orderRegister.
                      // Otherwise in case of a quick Controller restart, CoupleController would response with this OrderId
                      // and the Controller will try again to DetachOrder, while the original DetachOrder is still in progress.
                promise.future.map(_ => Right(AgentCommand.Response.Accepted))

          case None =>
            // May occur after Controller restart when Controller is not sure about order has been detached previously.
            logger.debug(s"Ignoring duplicate $cmd")
            Future.successful(Right(AgentCommand.Response.Accepted))

    case MarkOrder(orderId, mark) =>
      orderRegister.checked(orderId) match
        case Left(problem) =>
          Future.failed(problem.throwable)
        case Right(orderEntry) =>
          if orderEntry.isDetaching then
            Future.successful(Right(AgentCommand.Response.Accepted))
          else
            orderEventSource.markOrder(orderId, mark) match
              case Left(problem) => Future.failed(problem.throwable)
              case Right(Nil) => Future.successful(Right(AgentCommand.Response.Accepted))
              case Right(events) =>
                val sender = this.sender()
                // Several MarkOrder in sequence are not properly handled
                // one after the other because execution is asynchronous.
                // A second command may may see the same not yet updated order.
                // TODO Queue for each order? And no more OrderActor?
                IO
                  .deferFuture(
                    (orderEntry.actor ? OrderActor.Command.HandleEvents(events, CorrelId.current))
                      .mapTo[Checked[Completed]])
                  .flatMap:
                    case Left(problem)
                      if problem.exists(_.isInstanceOf[InapplicableOrderEventProblem]) =>
                      IO.sleep(100.ms) // brake
                        .*>(IO.defer {
                          logger.warn(s"Repeating $cmd due to race condition: $problem")
                          val promise = Promise[Checked[Response]]()
                          self.!(Input.ExternalCommand(cmd, CorrelId.current, promise))(sender)
                          IO
                            .fromFuture(IO.pure(promise.future))
                            .map(_.map(_.asInstanceOf[Response.Accepted]))
                        })

                    case Left(problem) =>
                      // Should not happen. Controller does not handle the problem.
                      logger.warn(s"$cmd => $problem")
                      IO.left(problem)

                    case Right(Completed) =>
                      IO.right(AgentCommand.Response.Accepted)
                .unsafeToFuture()

    case ReleaseEvents(after) =>
      if shuttingDown then
        Future.failed(AgentIsShuttingDown.throwable)
      else
        val userId = controllerId.toUserId
        val current = journalState.userIdToReleasedEventId(userId)  // Must contain userId
        if after < current then
          Future(Left(ReverseReleaseEventsProblem(requestedUntilEventId = after, currentUntilEventId = current)))
        else
          persist(JournalEventsReleased(userId, after)):
            case (Stamped(_,_, _ <-: event), journaledState) =>
              journalState.applyEvent(event).map: o =>
                journalState = o
                AgentCommand.Response.Accepted

  private def createJobEntries(workflow: Workflow, zone: ZoneId): Unit =
    for (jobKey, job) <- workflow.keyToJob do
      if job.agentPath == ownAgentPath then
        jobRegister.insert(jobKey, new JobEntry(jobKey, job, zone))

  private def attachOrder(order: Order[Order.IsFreshOrReady]): Future[Completed] =
    val actor = newOrderActor(order.id)
    orderRegister.insert(order.id, actor)
    (actor ? OrderActor.Command.Attach(order, CorrelId.current)).mapTo[Completed]  // TODO ask will time-out when Journal blocks
    // Now expecting OrderEvent.OrderAttachedToAgent

  private def newOrderActor(orderId: OrderId) =
    watch(actorOf(
      OrderActor.props(
        orderId, CorrelId.current, subagentKeeper, journalActor = journalActor, journalConf),
      name = uniqueActorName(encodeAsActorName("Order:" + orderId.string))))

  private def handleOrderEvent(
    previousOrder: Order[Order.State],
    event: OrderEvent)
  : Unit =
    // updatedOrderId may be outdated, changed by more events in the same batch
    // Nevertheless, updateOrderId is the result of the event.
    val orderId = previousOrder.id
    val agentState = this.agentState()
    val orderEventHandler = new OrderEventHandler(agentState.idToWorkflow.checked)

    orderEventHandler.handleEvent(previousOrder, event)
      .onProblem(problem => logger.error(
        s"handleOrderEvent($orderId <-: ${event.getClass.simpleScalaName}) => $problem)"))
      .foreach(_ foreach {
        case FollowUp.LeaveJob(jobKey) =>
          jobRegister
            .checked(jobKey)
            .onProblem(problem => logger.error(
              s"handleOrderEvent($orderId <-: ${event.getClass.simpleScalaName}) => $problem)"))
            .foreach { jobEntry =>
              jobEntry.queue.remove(orderId, dontWarn = true)
            }

        case FollowUp.AddChild(childOrder) =>
          val actor = newOrderActor(childOrder.id)
          orderRegister.insert(childOrder.id, actor)
          actor ! OrderActor.Input.AddChild(childOrder)
          proceedWithOrder(childOrder)

        case FollowUp.Delete(deleteOrderId) =>
          deleteOrder(deleteOrderId)
      })

  private def proceedWithOrder(orderId: OrderId): Unit =
    agentState().idToOrder.checked(orderId) match
      case Left(problem) => logger.error(s"Internal: proceedWithOrder($orderId) => $problem")
      case Right(order) => proceedWithOrder(order)

  private def proceedWithOrder(order: Order[Order.State]): Unit =
    if order.isAttached then
      val delayed = clock.lock:
        order.maybeDelayedUntil match
          case Some(until) if clock.now() < until =>
            // TODO Schedule only the next order ?
            val orderEntry = orderRegister(order.id)
            orderEntry.timer := clock.scheduleAt(until, s"Due(${order.id})"):
              self ! Internal.Due(order.id)
            true

          case _ =>
            false

      if !delayed then
        val agentState = this.agentState()
        val oes = new OrderEventSource(agentState)
        if order != agentState.idToOrder(order.id) then
          // FIXME order should be equal !
          logger.debug(s"❌ ERROR order    =$order")
          logger.debug(s"❌ ERROR idToOrder=${agentState.idToOrder(order.id)}")
          //assertThat(oes.state.idToOrder(order.id) == order)

        val keyedEvents: Seq[KeyedEvent[OrderActorEvent | PlanFinishedEvent]] =
          oes.nextEvents(order.id)
        val (orderKeyedEvents, noticeDeletedEvents) = keyedEvents.partitionMap:
          case o @ KeyedEvent(_, _: OrderActorEvent) =>
            Left(o.asInstanceOf[KeyedEvent[OrderActorEvent]])
          case o @ KeyedEvent(_, _: PlanFinishedEvent) =>
            Right(o.asInstanceOf[KeyedEvent[PlanFinishedEvent]])

        val future = orderKeyedEvents
          .groupMap(_.key)(_.event)
          .toSeq
          .parTraverse((orderId_ : OrderId, events) =>
            IO
              .fromFuture(IO:
                (orderRegister(orderId_).actor ?
                  OrderActor.Command.HandleEvents(events, CorrelId.current))
                    .mapTo[Checked[Completed]])
              .materializeIntoChecked
              .map(orderId_ -> events -> _))
          .unsafeToFuture()
        // TODO Not awaiting the response may lead to duplicate events
        //  for example when OrderSuspensionMarked is emitted after OrderProcessed and before OrderMoved.
        //  Then, two OrderMoved are emitted, because the second event is based on the same Order state.
        // TODO Blocking! SLOW because inhibits parallelization
        try Await.result(future, 99.s)
          .collect { case ((orderId_, events), Left(problem)) =>
            logger.error(
              s"$orderId_ <-: ${events.map(_.toShortString)} => $problem")
          }
        catch case NonFatal(t) => logger.error(
          s"${keyedEvents.map(_.toShortString)} => ${t.toStringWithCauses}")

        if keyedEvents.isEmpty
          && agentState.isOrderProcessable(order)
          && order.isAttached
          && !shuttingDown
        then
          onOrderIsProcessable(order)

        if noticeDeletedEvents.nonEmpty then
          persist(EventCalc.pure(orderKeyedEvents)) { _ => }

  private def onOrderIsProcessable(order: Order[Order.State]): Unit =
    agentState()
      .idToWorkflow.checked(order.workflowId)
      .map(workflow => workflow -> workflow.instruction(order.position))
      .match
        case Left(problem) =>
          logger.error(s"onOrderIsProcessable => $problem")

        case Right((workflow, execute: Execute)) =>
          val checkedJobKey = execute match
            case _: Execute.Anonymous => Right(workflow.anonymousJobKey(order.workflowPosition))
            case o: Execute.Named     => workflow.jobKey(order.position.branchPath, o.name)  // defaultArguments are extracted later
          checkedJobKey
            .flatMap(jobRegister.checked)
            .onProblem(problem =>
              logger.error(s"Internal: onOrderIsProcessable(${order.id}) => $problem"))
            .foreach { jobEntry =>
              onOrderAvailableForJob(order.id, jobEntry)
            }

        case Right(_) =>

  private def onOrderAvailableForJob(orderId: OrderId, jobEntry: JobEntry): Unit =
    // TODO Make this more functional!
    if !jobEntry.queue.isKnown(orderId) then
      jobEntry.queue += orderId
      tryStartProcessing(jobEntry)

  private def tryStartProcessing(): Unit =
    val it = jobRegister.valuesIterator
    while it.hasNext && tryStartProcessing(it.next()) do {}

  private def tryStartProcessing(jobEntry: JobEntry): Boolean =
    lazy val isEnterable = jobEntry.checkAdmissionTimeInterval:
      self ! Internal.JobDue(jobEntry.jobKey)

    val idToOrder = agentState().idToOrder

    @tailrec def loop(): Boolean =
      (jobEntry.isBelowProcessLimit && isBelowAgentProcessLimit()) && (
        jobEntry.queue.dequeueWhere(orderId =>
          idToOrder.get(orderId).exists(_.forceJobAdmission) || isEnterable)
        match
          case None => true
          case Some(orderId) =>
            startProcessing(orderId, jobEntry)
            loop())
    loop()

  private def startProcessing(orderId: OrderId, jobEntry: JobEntry): Unit =
    orderRegister.checked(orderId) match
      case Left(problem) =>
        logger.error(s"onOrderIsProcessable => $problem")

      case Right(orderEntry) =>
        jobEntry.processCount += 1
        agentProcessCount += 1
        orderEntry.actor ! OrderActor.Input.StartProcessing

  private def deleteOrder(orderId: OrderId): Unit =
    for orderEntry <- orderRegister.get(orderId) do
      orderEntry.actor ! OrderActor.Input.Terminate()
      orderRegister.remove(orderId)

  private def switchOver(cmd: AgentCommand): Future[Checked[AgentCommand.Response]] =
    IO
      .defer:
        logger.info(s"❗️ $cmd")
        switchingOver = true // Asynchronous !!!
        // SubagentKeeper stops the local (surrounding) Subagent,
        // which lets the Director (RunningAgent) stop
        subagentKeeper.stop.as(Right(()))
          .flatMapT(_ => workingClusterNode.switchOver)
          .flatMapT(_ => IO.right(self ! Internal.ContinueSwitchover))
          .rightAs(AgentCommand.Response.Accepted)
      .unsafeToFuture()

  override def unhandled(message: Any): Unit =
    message match
      case Internal.JobDriverStopped =>
        logger.trace("Internal.JobDriverStopped")
        jobRegister.values.foreach(_.close())
        jobRegister.keys.toVector.foreach(jobRegister.remove)
        shutdown.continue()

      case Terminated(actorRef) if orderRegister.contains(actorRef) =>
        val orderEntry = orderRegister(actorRef)
        val orderId = orderEntry.orderId
        logger.debug(s"Actor '$orderId' stopped")
        for p <- orderEntry.detachResponses do p.trySuccess(())
        orderRegister.onActorTerminated(actorRef)  // Delete the OrderEntry
        shutdown.continue()

      case Internal.JournalStopped if shuttingDown =>
        context.stop(self)

      case Internal.ContinueSwitchover =>
        val shutdownCmd = AgentCommand.ShutDown(
          clusterAction = Some(AgentCommand.ShutDown.ClusterAction.Switchover),
          restartDirector = true)
        shutDownOnce.trySet(shutdownCmd)
        shutdown.start(shutdownCmd)

      case Internal.AgentShutdown =>
        shutdown.finallyShutdown()

      case _ =>
        super.unhandled(message)

  private def orderEventSource =
    new OrderEventSource(agentState())

  private def isBelowAgentProcessLimit() =
    agentProcessLimit().forall(agentProcessCount < _)

  private def agentProcessLimit(): Option[Int] =
    agentState().keyToItem(AgentRef).get(ownAgentPath) match
      case None =>
        logger.warn("Missing own AgentRef — assuming processLimit = 0")
        Some(0)
      case Some(agentRef) =>
        agentRef.processLimit

  private def agentState(): AgentState =
    journal.unsafeAggregate()

  override def toString = "AgentOrderKeeper"


object AgentOrderKeeper:
  private val logger = Logger[this.type]

  sealed trait Input
  object Input:
    final case class ExternalCommand(
      command: AgentCommand,
      correlId: CorrelId,
      response: Promise[Checked[Response]])
    case object ResetAllSubagents

  private object Internal:
    final case class Recover(agentState: AgentState)
    final case class SubagentKeeperInitialized(agentState: AgentState, tried: Try[Unit])
    final case class OrdersRecovered(agentState: AgentState)
    final case class Due(orderId: OrderId)
    final case class JobDue(jobKey: JobKey)
    case object TryStartProcessing
    case object JobDriverStopped
    case object ContinueSwitchover
    case object AgentShutdown
    case object JournalStopped

  private final class JobEntry(
    val jobKey: JobKey,
    val workflowJob: WorkflowJob,
    zone: ZoneId):

    val queue = new OrderQueue
    private val admissionTimeIntervalSwitch = new ExecuteAdmissionTimeSwitch(
      workflowJob.admissionTimeScheme.getOrElse(AdmissionTimeScheme.always),
      zone,
      onSwitch = to =>
        if !to.contains(TimeInterval.Always) then
          logger.debug(s"$jobKey: Next admission: ${to getOrElse "None"} $zone"))

    var processCount = 0

    def close(): Unit =
      admissionTimeIntervalSwitch.cancel()

    def recoverProcessingOrder(order: Order[Order.Processing]): Unit =
      processCount += 1
      queue.recoverProcessingOrder(order)

    def checkAdmissionTimeInterval(onPermissionGranted: => Unit)(using AlarmClock): Boolean =
      admissionTimeIntervalSwitch.updateAndCheck(onPermissionGranted)

    def isBelowProcessLimit =
      processCount < workflowJob.processLimit

  final class OrderQueue private[order]:
    private val queue = mutable.ListBuffer.empty[OrderId]
    private val queueSet = mutable.Set.empty[OrderId]
    private val inProcess = mutable.Set.empty[OrderId]

    def isEmpty: Boolean = queue.isEmpty
    def nonEmpty: Boolean = !isEmpty

    def isKnown(orderId: OrderId): Boolean =
      queueSet.contains(orderId) || inProcess.contains(orderId)

    def dequeueWhere(predicate: OrderId => Boolean): Option[OrderId] =
      queue.nonEmpty.thenSome:
        queue.indexWhere(predicate) match
          case -1 => None
          case i =>
            val orderId = queue.remove(i)
            queueSet -= orderId
            inProcess += orderId
            Some(orderId)
      .flatten

    def +=(orderId: OrderId): Unit =
      if inProcess(orderId) then throw new DuplicateKeyException(s"Duplicate $orderId")
      if queueSet contains orderId then throw new DuplicateKeyException(s"Duplicate $orderId")
      queue += orderId
      queueSet += orderId

    def recoverProcessingOrder(order: Order[Order.Processing]): Unit =
      inProcess += order.id

    def remove(orderId: OrderId, dontWarn: Boolean = false): Unit =
      if !inProcess.remove(orderId) then
        val s = queue.size
        queue -= orderId
        if !dontWarn && queue.size == s then
          logger.warn(s"JobRegister.OrderQueue: unknown $orderId")
        queueSet -= orderId

    override def toString = s"OrderQueue(${queue.size} orders, ${inProcess.size} in process)"
