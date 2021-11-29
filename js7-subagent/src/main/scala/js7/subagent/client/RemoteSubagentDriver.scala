package js7.subagent.client

import akka.actor.ActorSystem
import cats.syntax.traverse._
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.crypt.Signed
import js7.base.generic.Completed
import js7.base.io.https.HttpsConfig
import js7.base.io.process.{ProcessSignal, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.monixutils.AsyncMap
import js7.base.monixutils.MonixBase.syntax.{RichMonixObservable, RichMonixTask}
import js7.base.monixutils.ObservablePauseDetector._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.stream.{Numbered, ObservableNumberedQueue}
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.ScalaUtils.syntax.{RichAny, RichEitherF, RichPartialFunction, RichString}
import js7.base.web.HttpClient
import js7.common.http.RecouplingStreamReader
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.event.JournalEvent.StampedHeartbeat
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, JournaledState, KeyedEvent, Stamped}
import js7.data.item.SignableItem
import js7.data.job.JobResourcePath
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.state.StateView
import js7.data.subagent.SubagentRef
import js7.data.value.expression.Expression
import js7.journal.state.StatePersistence
import js7.launcher.StdObservers
import js7.subagent.SubagentState.keyedEventJsonCodec
import js7.subagent.client.RemoteSubagentDriver._
import js7.subagent.data.SubagentCommand
import js7.subagent.data.SubagentCommand.{AttachItem, AttachSignedItem, DedicateSubagent, KillProcess, StartOrderProcess}
import monix.eval.Task
import monix.execution.atomic.Atomic
import monix.execution.cancelables.SerialCancelable
import monix.reactive.Observable
import scala.concurrent.Promise

final class RemoteSubagentDriver[S <: JournaledState[S] with StateView](
  val subagentRef: SubagentRef,
  userAndPassword: Option[UserAndPassword],
  httpsConfig: HttpsConfig,
  persistence: StatePersistence[S],
  agentPath: AgentPath,
  controllerId: ControllerId,
  protected val conf: SubagentDriver.Conf,
  recouplingStreamReaderConf: RecouplingStreamReaderConf,
  actorSystem: ActorSystem)
extends SubagentDriver
{
  def subagentId = subagentRef.id

  private val logger = Logger.withPrefix[this.type](subagentId.string)
  private val commandQueue = new ObservableNumberedQueue[SubagentCommand]

  private val client = new SubagentClient(
    Admission(subagentRef.uri, userAndPassword),
    httpsConfig,
    name = subagentRef.id.toString,
    actorSystem)

  private val commandPosting = SerialCancelable()
  private val eventFetcher = newEventFetcher()
  private val orderToProcessing =
    new AsyncMap(Map.empty[OrderId, Processing]) with AsyncMap.WhenEmpty

  @volatile private var stopping = false
  private val _isAlive = Atomic(false)

  def isAlive = _isAlive.get()

  private def setAlive(alive: Boolean): Unit =
    if (_isAlive.getAndSet(alive) != alive) {
      logger.info(subagentId.toString + (if (isAlive) " is alive" else " is not alive"))
    }

  def start: Task[Unit] =
    commandQueue.enqueue(
      DedicateSubagent(subagentId, agentPath, controllerId) :: Nil
    ) *>
      startCommandPosting *>
        startEventFetching

  def stop(signal: Option[ProcessSignal]): Task[Unit] =
    Task.defer {
      logger.debug("stop")
      stopping = true

      signal
        .fold(Task.unit)(killAll)
        .flatMap(_ => orderToProcessing.whenEmpty)
        .flatMap(_ => eventFetcher.terminateAndLogout)
        .flatMap(_ => Task(commandPosting.cancel()))
        .flatMap(_ => client.tryLogout)
        .logWhenItTakesLonger(s"RemoteSubagentDriver($subagentId).stop")
        .map { _ => logger.debug("Stopped") }
    }

  private def killAll(signal: ProcessSignal): Task[Unit] =
    commandQueue.enqueue(
      orderToProcessing.toMap.keys.view
        .map(KillProcess(_, signal)))

  private def ifNotStopping: Task[Checked[Unit]] =
    Task(
      if (stopping)
        Left(Problem(s"RemoteSubagentDriver '$subagentId' is stopping"))
      else
        Checked.unit)

  private def startCommandPosting: Task[Unit] =
    Task.deferAction(scheduler =>
      if (stopping)
        Task.unit
      else Task {
        commandPosting := commandQueue
          .observable(after = 0)
          .map(_.orThrow) // TODO
          .flatMap(_
            .mapEval(numbered =>
              HttpClient.liftProblem(
                client.retryUntilReachable()(
                  client.loginUntilReachable(onlyIfNotLoggedIn = true) >>
                    client.executeSubagentCommand(numbered))))
            .map {
              case Left(problem) => logger.error(problem.toString)  // TODO
              case Right(response) => response // Ignore ?
            }
            .completedL
          )
          .runToFuture(scheduler)
      })

  // TODO Call this to update a changed JobResource
  def onSignedItemChanged(signedItem: Signed[SignableItem]): Task[Unit] =
    Task.defer {
      if (stopping)
        Task.unit
      else
        commandQueue.enqueue(AttachSignedItem(signedItem) :: Nil)
    }

  // TODO Duplicate
  private val onStdouterr: (OrderId, StdoutOrStderr, String) => Task[Unit] =
    (orderId, outerr, chunk) =>
      persistence
        .persistKeyedEvent(orderId <-: OrderStdWritten(outerr)(chunk) /*TODO delay = stdoutCommitDelay, commitEarly=!stdoutCommitDelay.isZero*/)
        .map(_.orThrow/*???*/)

  def processOrder(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[Outcome] =
    ifNotStopping.flatMapT(_ =>
      withStdObservers(order.id, keepLastErrLine = true/*failOnErrWritten???*/, onStdouterr)(stdObservers =>
        Task.defer {
          val orderId = order.id
          val processing = new Processing(stdObservers)
          orderToProcessing
            .insert(orderId, processing)
            .flatMapT(_ => Task.pure(persistence.currentState.idToWorkflow.checked(order.workflowId)))
            .flatMapT(workflow =>
              Task.defer {
                val cmd = StartOrderProcess(order, defaultArguments)
                val jobResourcePaths: Seq[JobResourcePath] = Nil // FIXME
                jobResourcePaths
                  .traverse(persistence.currentState.pathToJobResource.checked)
                  .map(_  :+ workflow)
                  .match_ {
                    case Left(problem) =>
                      Task.pure(Right(Outcome.Failed.fromProblem(problem)))

                    case Right(items) =>
                      commandQueue.enqueue(items.view.map(AttachItem(_)) :+ cmd) >>
                        Task
                          .fromFuture(processing.completed.future)
                          .map {
                            // What should we do with Outcome.Disrupt ???
                            case Outcome.Disrupted(reason) => Right(Outcome.Failed(Some(reason.toString)))
                            case o: Outcome.Completed => Right(o)
                          }
                  }
                })
          })).map(Outcome.Completed.fromChecked)

  def killProcess(orderId: OrderId, signal: ProcessSignal) =
    commandQueue.enqueue(KillProcess(orderId, signal) :: Nil)

  def commandObservable(after: Long): Task[Checked[Observable[Numbered[SubagentCommand]]]] =
    ifNotStopping.flatMapT(_ =>
      commandQueue.observable(after))

  def releaseUntil(after: Long): Task[Checked[Unit]] =
    ifNotStopping.flatMapT(_ =>
      commandQueue.releaseUntil(after)
        .map(Right(_)))

  private def startEventFetching: Task[Unit] =
    Task.deferAction(scheduler => Task {
      if (!stopping) {
        eventFetcher
          .observe(client, after = EventId.BeforeFirst)
          .mapEval {
            // Wie liest man transaktions-sicher ???
            //  Solange nicht OrderProcessed in unserem Journal steht,
            //  müssen wir (nach Fehler/Wiederanlauf) die Events nochmal vom Subagenten lesen.
            case Stamped(_, _, KeyedEvent(orderId: OrderId, event @ OrderStdWritten(outerr, chunk))) =>
              Task.defer(
                orderToProcessing.get(orderId)
                  .toRight(Problem.pure(s"Unknown $orderId for $event"))
                  .traverse {
                    _.stdObservers.taskObserver(outerr).send(chunk)
                  })

            case Stamped(_, _, KeyedEvent(orderId: OrderId, event @ OrderProcessed(outcome))) =>
              for (maybeProcessing <- orderToProcessing.remove(orderId)) yield
                maybeProcessing
                  .toRight(Problem.pure(s"Unknown $orderId for $event"))
                  .map(_.completed.success(outcome))

            case stamped =>
              Task.pure(Right(Problem.pure(s"Unknown event from $subagentId ignored: $stamped")))
          }
          .map {
            case Left(problem) => logger.error(problem.toString) // TODO Should we abort?
            case Right(_) =>
          }
          .completedL
          .runAsyncAndForget(scheduler)
      }
    })

  private def newEventFetcher() =
    new RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], SubagentClient](
      _.eventId, recouplingStreamReaderConf)
    {
      private var lastProblem: Option[Problem] = None
      override protected def idleTimeout = longHeartbeatTimeout * 2/*???*/

      //override protected def couple(eventId: EventId) = ???

      protected def getObservable(api: SubagentClient, after: EventId) =
        Task.defer {
          logger.debug(s"getObservable(after=$after)")
          api.login(onlyIfNotLoggedIn = true) >>
            api
              .eventObservable(
                EventRequest.singleClass[Event](after = after, timeout = None),
                heartbeat = Some(heartbeat))
              .map(_
                .doAfterSubscribe(Task {
                  setAlive(true)
                })
                .tapEach(o => Task(logger.debug(o.toString.truncateWithEllipsis(200))))
                .detectPauses(longHeartbeatTimeout, EmptyStamped)
                .flatMap(o =>
                  if (o eq EmptyStamped) {
                    setAlive(false)
                    Observable.empty
                  } else {
                    setAlive(true)
                    Observable.pure(o)
                  })
                .filter(_ != StampedHeartbeat)
                .guarantee(Task {
                  setAlive(false)
                }))
              .map(Right(_))
        }

      override protected def onCouplingFailed(api: SubagentClient, problem: Problem) =
        Task {
          if (lastProblem contains problem) {
            logger.debug(s"Coupling failed: $problem")
            true
          } else {
            lastProblem = Some(problem)
            logger.warn(s"Coupling failed: $problem")
            if (noJournal)
              true
            else {
              logger.error(s"onCouplingFailed $problem")
              //eventObserver.persist(subagentId <-: SubagentCouplingFailed(problem))
              //  .map(_ => true)  // recouple and continue after onCouplingFailed
              //  .orThrow
              true
            }
          }
        }

      override protected def onCoupled(api: SubagentClient, after: EventId) =
        Task {
          logger.info(s"Coupled with $api after=${EventId.toString(after)}")
          Completed
        }

      override protected def onDecoupled =
        Task {
          logger.debug("onDecoupled")
          Completed
        }

      protected def stopRequested = false
    }

  override def toString =
    s"RemoteSubagentDriver(${subagentId.string})"
}

object RemoteSubagentDriver
{
  private val noJournal = false
  private val EmptyStamped: Stamped[KeyedEvent[Event]] = Stamped(0L, null)
  private val heartbeat = 3.s // TODO
  private val heartbeatTimeout = 10.s // TODO
  private val longHeartbeatTimeout = heartbeat + heartbeatTimeout

  private final class Processing(val stdObservers: StdObservers) {
    val completed = Promise[Outcome]()
  }
}
