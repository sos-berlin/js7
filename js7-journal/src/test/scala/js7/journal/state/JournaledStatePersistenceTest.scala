package js7.journal.state

import akka.pattern.ask
import akka.util.Timeout
import io.circe.generic.JsonCodec
import java.nio.file.Files.createTempDirectory
import java.nio.file.Path
import java.util.concurrent.Executors
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.generic.GenericString
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.utils.Collections.implicits._
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkautils.ProvideActorSystem
import js7.common.configutils.Configs._
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.scalautil.FileUtils.deleteDirectoryRecursively
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.data.cluster.ClusterState
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, JournalEvent, JournalState, JournaledState, JournaledStateBuilder, KeyedEvent, KeyedEventTypedJsonCodec, Stamped}
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalMeta
import js7.journal.recover.JournaledStateRecoverer
import js7.journal.state.JournaledStatePersistenceTest._
import js7.journal.test.TestData
import js7.journal.watch.JournalEventWatch
import js7.journal.{EventIdClock, EventIdGenerator, JournalActor, StampedKeyedEventBus}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import shapeless.tag

/**
  * @author Joacim Zschimmer
  */
final class JournaledStatePersistenceTest extends AnyFreeSpec with BeforeAndAfterAll with ProvideActorSystem
{
  coupleScribeWithSlf4j()

  private implicit lazy val scheduler = Scheduler(Executors.newCachedThreadPool())  // Scheduler.Implicits.global blocks on 2-processor machine
  protected def config = TestConfig
  protected lazy val directory = createTempDirectory("JournaledStatePersistenceTest-")
  private lazy val journalMeta = testJournalMeta(fileBase = directory)

  override def afterAll() = {
    close()
    deleteDirectoryRecursively(directory)
    scheduler.shutdown()
    super.afterAll()
  }

  private val n = 1000
  private val keys = for (o <- 'A' to 'D') yield NumberKey(o.toString)
  private val expectedThingCollection = NumberThingCollection(
    Vector(
      NumberThing(NumberKey("ONE"), 0),
      NumberThing(NumberKey("TWO"), 0)
    ).toKeyedMap(_.key) ++
      keys.toVector
        .map(key => key -> NumberThing(key, n * 1000))
        .toMap)

  "First run" - {
    lazy val runningPersistence = new RunningPersistence
    lazy val persistence = runningPersistence.start()

    "Start" in {
       persistence
    }

    "persistKeyedEvent with simple KeyedEvent" in {
      assert(persistence.persistKeyedEvent(NumberKey("ONE") <-: NumberAdded).runSyncUnsafe().isRight)
      assert(persistence.persistKeyedEvent(NumberKey("ONE") <-: NumberAdded).runSyncUnsafe() ==
        Left(Problem("Event 'ONE <-: NumberAdded' cannot be applied: Duplicate NumberThing: ONE")))
      intercept[MatchError] { persistence.persistKeyedEvent(NumberKey("ONE") <-: NumberUnhandled).runSyncUnsafe() }
    }

    "persistEvent" in {
      assert(persistence.persistEvent[NumberEvent](NumberKey("TWO"))(_ => Right(NumberAdded)).runSyncUnsafe().isRight)
    }

    "Concurrent update" in {
      val updated = keys
        .map(key =>
          persistence.persistKeyedEvent(key <-: NumberAdded)
            .runToFuture: Future[Checked[(Stamped[KeyedEvent[TestEvent]], TestState)]])
        .await(99.s)
      assert(updated.collectFirst { case Left(problem) => problem }.isEmpty)

      val keyFutures = for (key <- keys) yield
        Future {
          for (i <- 0 until n) yield
            persistence.persistKeyedEvent(key <-: NumberSlowlyIncremented(i * 1000))
              .runToFuture.await(99.s)
        }
      assert(keyFutures.await(99.s).flatten.collectFirst { case Left(problem) => problem }.isEmpty)
    }

    "currentState" in {
      assert(persistence.currentState.await(99.s) == TestState(eventId = 1000000 + 2 + keys.size * (1 + n), expectedThingCollection))
    }

    "Stop" in {
      runningPersistence.stop()
    }
  }

  "Second run, with recovered state" - {
    lazy val runningPersistence = new RunningPersistence
    lazy val persistence = runningPersistence.start()

    "Start" in {
      persistence
    }

    "currentState" in {
      assert(persistence.currentState.await(99.s) == TestState(eventId = 1000000 + 4 + keys.size * (1 + n), expectedThingCollection))
    }

    "Stop" in {
      runningPersistence.stop()
    }
  }

  private class RunningPersistence {
    private var journaledStatePersistence: JournaledStatePersistence[TestState] = null
    private lazy val journalStopped = Promise[JournalActor.Stopped]()

    private lazy val journalActor = tag[JournalActor.type](
      actorSystem.actorOf(
        JournalActor.props[TestState](journalMeta, JournalConf.fromConfig(config), new StampedKeyedEventBus, Scheduler.global,
          new EventIdGenerator(new EventIdClock.Fixed(currentTimeMillis = 1000/*EventIds start at 1000000*/)),
          journalStopped)))

    def start() = {
      val recovered = JournaledStateRecoverer.recover[TestState](journalMeta, JournalEventWatch.TestConfig)
      recovered.startJournalAndFinishRecovery(journalActor)(actorSystem)
      implicit val a = actorSystem
      implicit val timeout = Timeout(99.s)
      journaledStatePersistence = new JournaledStatePersistence[TestState](journalActor, JournalConf.fromConfig(TestConfig))
      journaledStatePersistence.start(recovered.recoveredState getOrElse TestState.empty)
      journaledStatePersistence
    }

    def stop() = {
      (journalActor ? JournalActor.Input.TakeSnapshot)(99.seconds) await 99.seconds
      if (journaledStatePersistence != null) {
        journaledStatePersistence.close()
      }
      journalActor ! JournalActor.Input.Terminate
      journalStopped.future await 99.s
    }
  }
}

private object JournaledStatePersistenceTest
{
  private val TestConfig = TestData.TestConfig
    .withFallback(config"""
      js7.akka.actor-message-log-level = Trace
      js7.journal.users-allowed-to-release-events = []
      js7.journal.release-events-delay = 0s
      js7.journal.remove-obsolete-files = true
      js7.journal.dispatcher.type = PinnedDispatcher
      """)

  private class TestStateBuilder extends JournaledStateBuilder[TestState]
  {
    private val numberThings = mutable.Map[NumberKey, NumberThing]()
    private var _state = TestState.empty

    protected def onInitializeState(state: TestState) = throw new NotImplementedError

    protected def onAddSnapshotObject = {
      case numberThing: NumberThing =>
        if (numberThings.contains(numberThing.key)) throw Problem(s"Duplicate NumberThing: ${numberThing.key}").throwable
        numberThings += numberThing.key -> numberThing
    }

    def onOnAllSnapshotsAdded() =
      _state = TestState(EventId.BeforeFirst, NumberThingCollection(numberThings.toMap))

    protected def onAddEvent: PartialFunction[Stamped[KeyedEvent[Event]], Unit] = {
      case Stamped(_, _, KeyedEvent(k: NumberKey, e: NumberEvent)) =>
        _state = _state.applyEvent(k <-: e).orThrow

      case Stamped(_, _, KeyedEvent(_, _: JournalEvent)) =>
    }

    def state =
      _state.copy(eventId = eventId)

    def journalState = JournalState.empty

    def clusterState = ClusterState.Empty
  }

  private def testJournalMeta(fileBase: Path) =
    JournalMeta(TestState, fileBase)

  final case class NumberThing(key: NumberKey, number: Int) {
    def update(event: NumberEvent): Checked[NumberThing] =
      event match {
        case NumberIncremented =>
          Right(copy(number = number + 1))

        case e @ NumberSlowlyIncremented(expected) =>
          if (number != expected)
            Left(Problem(s"$e, but number is $toString"))
          else {
            Thread.sleep(1)
            Right(copy(number = number + 1000))
          }

        case _ => throw new MatchError(event)
      }
  }

  final case class NumberThingCollection(numberThings: Map[NumberKey, NumberThing])
  {
    def applyEvent: PartialFunction[KeyedEvent[NumberEvent], Checked[NumberThingCollection]] = {
      case KeyedEvent(key: NumberKey, event: NumberEvent) =>
        event match {
          case NumberAdded =>
            if (numberThings.contains(key))
              Left(Problem(s"Duplicate NumberThing: $key"))
            else
              Right(copy(numberThings = numberThings + (key -> NumberThing(key, 0))))
          case _ =>
            numberThings.checked(key)
              .flatMap(_.update(event))
              .map(thing => copy(numberThings = numberThings + (thing.key -> thing)))
        }
    }
  }

  final case class StringThing(key: StringKey, string: String)

  @JsonCodec
  final case class NumberKey(string: String) extends GenericString

  @JsonCodec
  final case class StringKey(string: String) extends GenericString

  sealed trait TestEvent extends Event

  sealed trait NumberEvent extends TestEvent {
    type Key = NumberKey
  }
  object NumberEvent {
    implicit val jsonCodec = TypedJsonCodec[NumberEvent](
      Subtype(NumberAdded),
      Subtype(NumberRemoved),
      Subtype(NumberIncremented),
      Subtype(deriveCodec[NumberSlowlyIncremented]),
      Subtype(NumberUnhandled))
  }
  case object NumberAdded extends NumberEvent

  case object NumberRemoved extends NumberEvent

  case object NumberIncremented extends NumberEvent

  final case class NumberSlowlyIncremented(expected: Int) extends NumberEvent

  case object NumberUnhandled  extends NumberEvent

  final case class TestState(
    eventId: EventId,
    numberThingCollection: NumberThingCollection,
    standards: JournaledState.Standards = JournaledState.Standards.empty)
  extends JournaledState[TestState]
  {
    protected type Self = NumberThingCollection
    protected type Snapshot = NumberThing
    protected type E = NumberEvent

    def withEventId(eventId: EventId) =
      copy(eventId = eventId)

    def withStandards(standards: JournaledState.Standards) =
      copy(standards = standards)

    def applyEvent(keyedEvent: KeyedEvent[Event]) =
      keyedEvent match {
        case KeyedEvent(key: NumberKey, event: NumberEvent) =>
          for (o <- numberThingCollection.applyEvent(key <-: event)) yield
            copy(numberThingCollection = o)

        case keyedEvent => applyStandardEvent(keyedEvent)
      }

    def estimatedSnapshotSize = numberThingCollection.numberThings.size

    def toSnapshotObservable = Observable.fromIterable(numberThingCollection.numberThings.values)
  }
  object TestState extends JournaledState.Companion[TestState] {
    val empty = TestState(EventId.BeforeFirst, NumberThingCollection(Map.empty))

    def newBuilder(): JournaledStateBuilder[TestState] =
      new TestStateBuilder

    override def fromObservable(snapshotObjects: Observable[Any]): Task[TestState] =
      throw new NotImplementedError  // Require for HTTP EventApi only

    def snapshotObjectJsonCodec: TypedJsonCodec[Any] =
      TypedJsonCodec[Any](
        Subtype(deriveCodec[NumberThing]),
        Subtype(deriveCodec[StringThing]))


    override implicit def keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
      KeyedEventTypedJsonCodec[Event](
          KeyedSubtype[JournalEvent],
          KeyedSubtype[NumberEvent])
  }
}
