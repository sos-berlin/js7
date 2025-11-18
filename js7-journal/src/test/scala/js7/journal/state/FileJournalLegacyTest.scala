package js7.journal.state

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.Stream
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import java.nio.file.Files.createTempDirectory
import java.nio.file.Path
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.configutils.Configs.*
import js7.base.generic.GenericString
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.Collections.RichMap
import js7.base.utils.Collections.implicits.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkoutils.ProvideActorSystem
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, JournalEvent, KeyedEvent, KeyedEventTypedJsonCodec, SnapshotableState, SnapshotableStateRecoverer, Stamped}
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalLocation
import js7.journal.recover.StateRecoverer
import js7.journal.state.FileJournalLegacyTest.*
import js7.journal.test.TestData
import js7.journal.watch.JournalEventWatch
import js7.journal.{EventIdGenerator, FileJournal}
import org.apache.pekko.util.Timeout
import org.scalatest.BeforeAndAfterAll
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
final class FileJournalLegacyTest extends OurTestSuite, BeforeAndAfterAll:

  private given IORuntime = ioRuntime
  private given ExecutionContext = ioRuntime.compute

  protected lazy val directory: Path = createTempDirectory("FileJournalLegacyTest-")
  private lazy val journalLocation = testJournalMeta(fileBase = directory)

  override def afterAll() =
    try
      deleteDirectoryRecursively(directory)
    finally
      super.afterAll()

  private val n = 100
  private val keys = for o <- 'A' to 'D' yield NumberKey(o.toString)
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

    "Start" in:
       journal

    "persist with simple KeyedEvent" in:
      assert(journal.persist(NumberKey("ONE") <-: NumberAdded).unsafeRunSync().isRight)
      assert(journal.persist(NumberKey("ONE") <-: NumberAdded).unsafeRunSync() ==
        // TODO Would be better: Left(Problem("Event 'ONE <-: NumberAdded' cannot be applied to " +
        //                        "FileJournalLegacyTest.TestState: Duplicate NumberThing: ONE")))
        Left(Problem("Duplicate NumberThing: ONE")))
      assert:
        journal.persist(NumberKey("ONE") <-: NumberUnhandled).unsafeRunSync() ==
          Left(Problem("scala.MatchError: NumberUnhandled (of class js7.journal.state.FileJournalLegacyTest$NumberUnhandled$)"))

    "persist" in:
      assert:
        journal.persist[NumberEvent]:
          NumberKey("TWO") <-: NumberAdded
        .unsafeRunSync().isRight

    "Concurrent update" in:
      val updated = keys
        .map(key =>
          journal.persistOne(key <-: NumberAdded)
            .unsafeToFuture(): Future[Checked[(Stamped[KeyedEvent[TestEvent]], TestState)]])
        .await(99.s)
      assert(updated.collectFirst { case Left(problem) => problem }.isEmpty)

      val keyFutures = for key <- keys yield
        Future:
          for i <- 0 until n yield
            journal.persist(key <-: NumberSlowlyIncremented(i * 1000))
              .await(99.s)
      assert(keyFutures.await(99.s).flatten.collectFirst { case Left(problem) => problem }.isEmpty)

    "currentState" in:
      assert(journal.unsafeAggregate() ==
        TestState(eventId = 1000000 + 2 + keys.size * (1 + n), expectedThingCollection))

    "Stop" in:
      runningJournal.stop()
  }

  "Second run, with recovered state" - {
    lazy val runningJournal = new RunningJournal
    lazy val journal = runningJournal.start()

    "Start" in:
      journal

    "currentState" in:
      assert(journal.unsafeAggregate() ==
        TestState(eventId = 1000000 + 4 + keys.size * (1 + n), expectedThingCollection))

    "Stop" in:
      runningJournal.stop()
  }

  private class RunningJournal(
    using protected val executionContext: ExecutionContext)
  extends ProvideActorSystem:
    import scala.language.unsafeNulls

    protected def config = TestConfig

    private var journalAllocated: Allocated[IO, FileJournal[TestState]] = null
    def journal = journalAllocated.allocatedThing

    def start() =
      val recovered = StateRecoverer.recover[TestState](journalLocation, JournalEventWatch.TestConfig)
      implicit val a = actorSystem
      implicit val timeout: Timeout = Timeout(99.s)
      journalAllocated = FileJournal
        .service(recovered, JournalConf.fromConfig(TestConfig),
          Some(EventIdGenerator.withFixedClock(epochMilli = 1000/*EventIds start at 1000000*/)))
        .toAllocated
        .await(99.s)
      journal

    def stop() =
      if journal != null then
        journalAllocated.release.await(99.s)
      journalAllocated.release.await(99.s)
      close()


private object FileJournalLegacyTest:

  private val TestConfig = TestData.TestConfig
    .withFallback(config"""
      js7.pekko.actor-message-log-level = Trace
      js7.journal.users-allowed-to-release-events = []
      js7.journal.release-events-delay = 0s
      js7.journal.remove-obsolete-files = true
      js7.journal.dispatcher.type = PinnedDispatcher
      """)

  private def testJournalMeta(fileBase: Path) =
    JournalLocation(TestState, fileBase)

  final case class NumberThing(key: NumberKey, number: Int):
    def update(event: NumberEvent): Checked[NumberThing] =
      event match
        case NumberIncremented =>
          Right(copy(number = number + 1))

        case e @ NumberSlowlyIncremented(expected) =>
          if number != expected then
            Left(Problem(s"$e, but number is $toString"))
          else
            Thread.sleep(1)
            Right(copy(number = number + 1000))

        case _ => throw new MatchError(event)

  final case class NumberThingCollection(numberThings: Map[NumberKey, NumberThing]):
    def applyEvent: PartialFunction[KeyedEvent[NumberEvent], Checked[NumberThingCollection]] =
      case KeyedEvent(key: NumberKey, event: NumberEvent) =>
        event match
          case NumberAdded =>
            if numberThings.contains(key) then
              Left(Problem(s"Duplicate NumberThing: $key"))
            else
              Right(copy(numberThings = numberThings + (key -> NumberThing(key, 0))))
          case _ =>
            numberThings.checked(key)
              .flatMap(_.update(event))
              .map(thing => copy(numberThings = numberThings + (thing.key -> thing)))

  final case class StringThing(key: StringKey, string: String)

  final case class NumberKey(string: String) extends GenericString
  object NumberKey:
    implicit val jsonCodec: Codec.AsObject[NumberKey] = deriveCodec

  final case class StringKey(string: String) extends GenericString
  object StringKey:
    implicit val jsonCodec: Codec.AsObject[StringKey] = deriveCodec

  sealed trait TestEvent extends Event

  sealed trait NumberEvent extends TestEvent, Event.IsKeyBase[NumberEvent]:
    val keyCompanion: NumberEvent.type = NumberEvent
  object NumberEvent extends Event.CompanionForKey[NumberKey, NumberEvent]:
    implicit val implicitSelf: NumberEvent.type = this
    implicit val jsonCodec: TypedJsonCodec[NumberEvent] = TypedJsonCodec(
      Subtype(NumberAdded),
      Subtype(NumberRemoved),
      Subtype(NumberIncremented),
      Subtype(deriveCodec[NumberSlowlyIncremented]),
      Subtype(NumberUnhandled))

  case object NumberAdded extends NumberEvent

  case object NumberRemoved extends NumberEvent

  case object NumberIncremented extends NumberEvent

  final case class NumberSlowlyIncremented(expected: Int) extends NumberEvent

  case object NumberUnhandled  extends NumberEvent


  final case class TestState(
    eventId: EventId,
    numberThingCollection: NumberThingCollection,
    standards: SnapshotableState.Standards = SnapshotableState.Standards.empty)
  extends SnapshotableState[TestState]:

    protected type Self = NumberThingCollection
    protected type Snapshot = NumberThing
    protected type E = NumberEvent

    def companion = TestState

    def name = "TestState"

    protected def withEventId_(eventId: EventId) =
      copy(eventId = eventId)

    def withStandards(standards: SnapshotableState.Standards) =
      copy(standards = standards)

    def applyKeyedEvent(keyedEvent: KeyedEvent[Event]) =
      keyedEvent match
        case KeyedEvent(key: NumberKey, event: NumberEvent) =>
          for o <- numberThingCollection.applyEvent(key <-: event) yield
            copy(numberThingCollection = o)

        case keyedEvent => applyStandardEvent(keyedEvent)

    def estimatedSnapshotSize = numberThingCollection.numberThings.size

    def toSnapshotStream = Stream.iterable(numberThingCollection.numberThings.values)


  object TestState extends SnapshotableState.Companion[TestState]:
    val empty = TestState(EventId.BeforeFirst, NumberThingCollection(Map.empty))

    def newRecoverer(): SnapshotableStateRecoverer[TestState] =
      new SnapshotableStateRecoverer.Simple(TestState):
        protected def onAddSnapshotObject =
          case numberThing: NumberThing =>
            updateState(result().copy(numberThingCollection = result().numberThingCollection.copy(
              numberThings =
                result().numberThingCollection.numberThings.insert(numberThing.key -> numberThing).orThrow)))

    protected val inventoryItems = Nil

    def snapshotObjectJsonCodec: TypedJsonCodec[Any] =
      TypedJsonCodec[Any](
        Subtype(deriveCodec[NumberThing]),
        Subtype(deriveCodec[StringThing]))

    override implicit def keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
      KeyedEventTypedJsonCodec[Event](
        KeyedSubtype[JournalEvent],
        KeyedSubtype[NumberEvent])
