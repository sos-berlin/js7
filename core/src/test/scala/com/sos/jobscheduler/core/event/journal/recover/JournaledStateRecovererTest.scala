package com.sos.jobscheduler.core.event.journal.recover

import JournaledStateRecovererTest._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.common.scalautil.FileUtils
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.core.event.state.JournaledStateBuilder
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, JournaledState, KeyedEvent, KeyedEventTypedJsonCodec, NoKeyEvent, Stamped}
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.typesafe.config.ConfigFactory
import java.nio.file.Paths
import monix.reactive.Observable
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
//  private val logger = com.sos.jobscheduler.common.scalautil.Logger(getClass)
//
//  private class TestStateBuilder extends JournaledStateBuilder[TestState]
//  {
//    private var _state = TestState(0)
//
//    protected def onInitializeState(state: TestState) = _state = state
//
//    protected def onAddSnapshot = {
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
//  implicit private val snapshotJsonCodec = TypedJsonCodec[Any](
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
