package js7.journal.metering

import cats.effect.kernel.Resource
import cats.effect.{IO, ResourceIO}
import java.nio.file.Path
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.config.Js7Config
import js7.base.io.file.FileUtils
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.log.LogLevel.Info
import js7.base.metering.Responsivenessmeter
import js7.base.problem.Checked
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, JournalEvent, JournalState, KeyedEvent, KeyedEventTypedJsonCodec, SnapshotableState, SnapshotableStateRecoverer}
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalLocation
import js7.journal.metering.ResponsivenessEvent.InternalResponseTime
import js7.journal.metering.ResponsivenessmeterTest.*
import js7.journal.recover.Recovered
import js7.journal.watch.EventWatch
import js7.journal.{FileJournal, metering}

final class ResponsivenessmeterTest extends OurAsyncTestSuite:

  "test" in:
    temporaryDirectoryResource[IO]("ResponsivenessmeterTest-").use: dir =>
      slowResponsivenessDetector(dir).use: (detector, eventWatch) =>
        eventWatch.awaitAsync[InternalResponseTime](_ => true, after = EventId.BeforeFirst, timeout = 99.s)
          .productR:
            IO:
              assert(detector.bean.getDelaySeconds > 0.0)

  private def slowResponsivenessDetector(dir: Path): ResourceIO[(Responsivenessmeter, EventWatch)] =
    for
      recovered =
        val loc = JournalLocation(MyAggregate, dir / "test")
        Recovered.noJournalFile[MyAggregate](loc, config)
      journal <- FileJournal.service[MyAggregate](recovered, JournalConf.fromConfig(config))
      service <- Responsivenessmeter.service(
        Responsivenessmeter.Conf(
          initialDelay = 0.s,
          meterInterval = 10.ms,
          slowThreshold = 1.ms,
          logLevel = Info,
          emitEvents = true,
          eventInterval = None))
      _ <- Resource.eval(service.onMetered(Responsiveness.onMetered(journal)))
    yield
      service -> recovered.eventWatch


object ResponsivenessmeterTest:
  private val config = Js7Config.defaultConfig


  private final case class MyAggregate(
    eventId: EventId,
    standards: SnapshotableState.Standards)
    extends SnapshotableState[MyAggregate]:

    def companion = MyAggregate

    def name = "MyAggregate"

    def toSnapshotStream = fs2.Stream.empty

    def estimatedSnapshotSize = 0

    def withStandards(standards: SnapshotableState.Standards) =
      copy(standards = standards)

    protected def withEventId_(eventId: EventId) =
      copy(eventId = eventId)

    def applyKeyedEvent(keyedEvent: KeyedEvent[Event]) =
      keyedEvent match
        case KeyedEvent(NoKey, _: ResponsivenessEvent.InternalResponseTime) => Right(this)
        case _ => applyStandardEvent(keyedEvent)

  private object MyAggregate extends SnapshotableState.Companion[MyAggregate]:
    val empty = MyAggregate(EventId.BeforeFirst, SnapshotableState.Standards.empty)

    def newRecoverer(): SnapshotableStateRecoverer[MyAggregate] =
      new SnapshotableStateRecoverer[MyAggregate]:
        protected val S = MyAggregate
        protected def onAddSnapshotObject = PartialFunction.empty
        def journalState: JournalState = JournalState.empty
        def clusterState: ClusterState = ClusterState.Empty
        def result() = MyAggregate.empty

    def keyedEventJsonCodec = KeyedEventTypedJsonCodec(
      KeyedSubtype[ResponsivenessEvent],
      KeyedSubtype[JournalEvent],
      KeyedSubtype[ClusterEvent])

    def snapshotObjectJsonCodec = TypedJsonCodec(
      Subtype[JournalState],
      Subtype[ClusterState])
