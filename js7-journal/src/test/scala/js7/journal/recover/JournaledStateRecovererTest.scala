package js7.journal.recover

import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournaledStateRecovererTest extends AnyFreeSpec
{
  //import JournaledStateRecovererTest.snapshotJsonCodec
  //import JournaledStateRecovererTest.eventJsonCodec
  //
  //"Test is missing" in {
  //  // TODO Test is missing
  //  // Lesen einer abgebrochenen Transaktion liefert korrekten Zustand und Datei-Position (vor der Transaktion)
  //  val config = ConfigFactory.empty
  //  FileUtils.withTemporaryFile("JournaledStateRecovererTest", ".tmp") { file =>
  //    val journalMeta = JournalMeta(snapshotJsonCodec, keyedEventJsonCodec, Paths.get(file.toString + "-test"))
  //    JournaledStateRecoverer.recover[TestState, TestEvent](journalMeta, newStateBuilder, config)
  //  }
  //}
}

private object JournaledStateRecovererTest
//{
//  private val logger = js7.common.scalautil.Logger(getClass)
//
//  private class TestStateBuilder extends JournaledStateBuilder[TestState]
//  {
//    private var _state = TestState(0)
//
//    protected def onInitializeState(state: TestState) = _state = state
//
//    protected def onAddSnapshotObject = {
//      case o: TestState => _state = o
//    }
//
//    protected def onOnAllSnapshotsAdded() = {}
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
//  final case class TestState(value: Int) extends JournaledState[TestState, Event]
//  {
//    def applyEvent(keyedEvent: KeyedEvent[Event]) =
//      keyedEvent match {
//        case NoKey <-: Add(number) => Right(copy(value + number))
//      }
//
//    def withEventId(eventId: EventId) = {}
//
//    def toSnapshotObservable: Observable[Any] =
//      Observable.pure(this)
//  }
//
//  sealed trait TestEvent extends NoKeyEvent
//  final case class Add(number: Int) extends TestEvent
//}
//
