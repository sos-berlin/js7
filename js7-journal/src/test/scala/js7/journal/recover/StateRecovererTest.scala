package js7.journal.recover

import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class StateRecovererTest extends OurTestSuite
{
  //import StateRecovererTest.snapshotObjectJsonCodec
  //import StateRecovererTest.eventJsonCodec
  //
  //"Test is missing" in {
  //  // TODO Test is missing
  //  // Lesen einer abgebrochenen Transaktion liefert korrekten Zustand und Datei-Position (vor der Transaktion)
  //  val config = ConfigFactory.empty
  //  FileUtils.withTemporaryFile("StateRecovererTest", ".tmp") { file =>
  //    val journalLocation = JournalLocation(snapshotObjectJsonCodec, keyedEventJsonCodec, Paths.get(file.toString + "-test"))
  //    StateRecoverer.recover[TestState, TestEvent](journalLocation, newStateBuilder, config)
  //  }
  //}
}

private object StateRecovererTest
//{
//  private val logger = js7.common.scalautil.Logger(getClass)
//
//  private class TestStateBuilder extends SnapshotableStateBuilder[TestState]
//  {
//    private var _state = TestState(0)
//
//    protected def onInitializeState(state: TestState) = _state = state
//
//    protected def onAddSnapshotObject = {
//      case o: TestState => _state = o
//    }
//
//    protected def onOnAllSnapshotsObjectsAdded() = {}
//
//    protected def onAddEvent = {
//      case Stamped(KeyedEvent(NoKey <-: event)) => _state = _state.apply()
//    }
//
//    def state = _state
//
//    def clusterState: ClusterState = ???
//  }
//
//  implicit private val snapshotObjectJsonCodec = TypedJsonCodec[Any](
//    Subtype(deriveCodec[TestState]))
//  implicit private val eventJsonCodec = TypedJsonCodec[TestEvent](
//    Subtype(deriveCodec[Add]))
//  implicit private val keyedEventJsonCodec = KeyedEventTypedJsonCodec[Event](
//    KeyedSubtype[TestEvent])
//
//  final case class TestState(value: Int) extends SnapshotableState[TestState, Event]
//  {
//    def applyKeyedEvent(keyedEvent: KeyedEvent[Event]) =
//      keyedEvent match {
//        case NoKey <-: Add(number) => Right(copy(value + number))
//      }
//
//    def withEventId(eventId: EventId) = {}
//
//    def toSnapshotStream: Stream[IO, Any] =
//      Stream.emit(this)
//  }
//
//  sealed trait TestEvent extends NoKeyEvent
//  final case class Add(number: Int) extends TestEvent
//}
//
