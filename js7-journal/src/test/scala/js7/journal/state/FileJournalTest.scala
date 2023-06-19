package js7.journal.state

import akka.pattern.ask
import akka.util.Timeout
import com.softwaremill.diffx.generic.auto.*
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import java.nio.file.Files.createTempDirectory
import java.nio.file.Path
import java.util.concurrent.Executors
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.configutils.Configs.*
import js7.base.generic.GenericString
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.Collections.RichMap
import js7.base.utils.Collections.implicits.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.akkautils.ProvideActorSystem
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, JournalEvent, KeyedEvent, KeyedEventTypedJsonCodec, SnapshotableState, SnapshotableStateBuilder, Stamped}
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalLocation
import js7.journal.recover.StateRecoverer
import js7.journal.state.FileJournalTest.*
import js7.journal.test.TestData
import js7.journal.watch.JournalEventWatch
import js7.journal.{EventIdClock, EventIdGenerator, JournalActor}
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable
import org.scalatest.BeforeAndAfterAll
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class FileJournalTest extends OurTestSuite with BeforeAndAfterAll
{
  coupleScribeWithSlf4j()

  private implicit lazy val scheduler: SchedulerService =
    Scheduler(Executors.newCachedThreadPool())  // Scheduler.Implicits.global blocks on 2-processor machine
  protected lazy val directory = createTempDirectory("FileJournalTest-")
  private lazy val journalLocation = testJournalMeta(fileBase = directory)

  override def afterAll() = {
    deleteDirectoryRecursively(directory)
    scheduler.shutdown()
    super.afterAll()
  }

  private val n = 100
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
    lazy val runningJournal = new RunningJournal
    lazy val journal = runningJournal.start()

    "Start" in {
       journal
    }

    "persistKeyedEvent with simple KeyedEvent" in {
      assert(journal.persistKeyedEvent(NumberKey("ONE") <-: NumberAdded).runSyncUnsafe().isRight)
      assert(journal.persistKeyedEvent(NumberKey("ONE") <-: NumberAdded).runSyncUnsafe() ==
        Left(Problem("Event 'ONE <-: NumberAdded' cannot be applied to " +
          "'FileJournalTest.TestState': Duplicate NumberThing: ONE")))
      intercept[MatchError] { journal.persistKeyedEvent(NumberKey("ONE") <-: NumberUnhandled).runSyncUnsafe() }
    }

    "persistEvent" in {
      assert(journal
        .persistEvent[NumberEvent](NumberKey("TWO"))(implicitly[sourcecode.Enclosing])(
          _ => Right(NumberAdded)).runSyncUnsafe().isRight)
    }

    "Concurrent update" in {
      val updated = keys
        .map(key =>
          journal.persistKeyedEvent(key <-: NumberAdded)
            .runToFuture: Future[Checked[(Stamped[KeyedEvent[TestEvent]], TestState)]])
        .await(99.s)
      assert(updated.collectFirst { case Left(problem) => problem }.isEmpty)

      val keyFutures = for (key <- keys) yield
        Future {
          for (i <- 0 until n) yield
            journal.persistKeyedEvent(key <-: NumberSlowlyIncremented(i * 1000))
              .runToFuture.await(99.s)
        }
      assert(keyFutures.await(99.s).flatten.collectFirst { case Left(problem) => problem }.isEmpty)
    }

    "currentState" in {
      assert(journal.unsafeCurrentState() ==
        TestState(eventId = 1000000 + 2 + keys.size * (1 + n), expectedThingCollection))
    }

    "Stop" in {
      runningJournal.stop()
    }
  }

  "Second run, with recovered state" - {
    lazy val runningJournal = new RunningJournal
    lazy val journal = runningJournal.start()

    "Start" in {
      journal
    }

    "currentState" in {
      assert(journal.unsafeCurrentState() ==
        TestState(eventId = 1000000 + 4 + keys.size * (1 + n), expectedThingCollection))
    }

    "Stop" in {
      runningJournal.stop()
    }
  }

  private class RunningJournal extends ProvideActorSystem {
    protected def config = TestConfig
    private var journalAllocated: Allocated[Task, FileJournal[TestState]] = null
    def journal = journalAllocated.allocatedThing

    def start() = {
      val recovered = StateRecoverer.recover[TestState](journalLocation, JournalEventWatch.TestConfig)
      implicit val a = actorSystem
      implicit val timeout: Timeout = Timeout(99.s)
      journalAllocated = FileJournal
        .resource(recovered, JournalConf.fromConfig(TestConfig),
          new EventIdGenerator(EventIdClock.fixed(epochMilli = 1000/*EventIds start at 1000000*/)))
        .toAllocated
        .await(99.s)
      journal
    }

    def stop() = {
      (journal.journalActor ? JournalActor.Input.TakeSnapshot)(99.s) await 99.s
      if (journal != null) {
        journalAllocated.release.await(99.s)
      }
      journalAllocated.release.await(99.s)
      close()
    }
  }
}

private object FileJournalTest
{
  private val TestConfig = TestData.TestConfig
    .withFallback(config"""
      js7.akka.actor-message-log-level = Trace
      js7.journal.users-allowed-to-release-events = []
      js7.journal.release-events-delay = 0s
      js7.journal.remove-obsolete-files = true
      js7.journal.dispatcher.type = PinnedDispatcher
      """)

  private def testJournalMeta(fileBase: Path) =
    JournalLocation(TestState, fileBase)

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

  final case class NumberKey(string: String) extends GenericString
  object NumberKey {
    implicit val jsonCodec: Codec.AsObject[NumberKey] = deriveCodec
  }

  final case class StringKey(string: String) extends GenericString
  object StringKey {
    implicit val jsonCodec: Codec.AsObject[StringKey] = deriveCodec
  }

  sealed trait TestEvent extends Event

  sealed trait NumberEvent extends TestEvent {
    type Key = NumberKey
  }
  object NumberEvent {
    implicit val jsonCodec: TypedJsonCodec[NumberEvent] = TypedJsonCodec(
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
    standards: SnapshotableState.Standards = SnapshotableState.Standards.empty)
  extends SnapshotableState[TestState]
  {
    protected type Self = NumberThingCollection
    protected type Snapshot = NumberThing
    protected type E = NumberEvent

    def companion = TestState

    def withEventId(eventId: EventId) =
      copy(eventId = eventId)

    def withStandards(standards: SnapshotableState.Standards) =
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
  object TestState extends SnapshotableState.Companion[TestState]
  {
    val empty = TestState(EventId.BeforeFirst, NumberThingCollection(Map.empty))

    def newBuilder(): SnapshotableStateBuilder[TestState] =
      new SnapshotableStateBuilder.Simple(TestState) {
        protected def onAddSnapshotObject = {
          case numberThing: NumberThing =>
            updateState(result().copy(numberThingCollection = result().numberThingCollection.copy(
              numberThings =
                result().numberThingCollection.numberThings.insert(numberThing.key -> numberThing).orThrow)))
        }
      }

    protected val inventoryItems = Nil

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
