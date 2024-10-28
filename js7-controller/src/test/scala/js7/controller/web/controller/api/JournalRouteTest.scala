package js7.controller.web.controller.api

import cats.effect.{Deferred, IO}
import cats.effect.unsafe.IORuntime
import java.nio.file.Files.{createTempDirectory, size}
import java.util.UUID
import js7.base.auth.SessionToken
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.monixlike.MonixLikeExtensions.{toListL, unsafeToCancelableFuture}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.Tests.isIntelliJIdea
import js7.base.utils.{CancelableFuture, Tests}
import js7.base.web.{HttpClient, Uri}
import js7.common.http.PekkoHttpClient
import js7.common.jsonseq.PositionAnd
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegments
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.session.SimpleSession
import js7.controller.web.controller.api.test.RouteTester
import js7.data.Problems.AckFromActiveClusterNodeProblem
import js7.data.controller.ControllerState
import js7.data.event.JournalEvent.SnapshotTaken
import js7.data.event.JournalSeparators.EndOfJournalFileMarker
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournalHeaders, JournalId, Stamped}
import js7.data.order.OrderEvent.OrderAdded
import js7.data.order.OrderId
import js7.data.workflow.WorkflowPath
import js7.journal.data.JournalLocation
import js7.journal.files.JournalFiles.*
import js7.journal.watch.JournalEventWatch
import js7.journal.web.JournalRoute
import js7.journal.write.{EventJournalWriter, SnapshotJournalWriter}
import js7.tester.ScalaTestUtils.awaitAndAssert
import org.apache.pekko.http.scaladsl.testkit.RouteTestTimeout
import scala.collection.mutable
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class JournalRouteTest extends OurTestSuite, RouteTester, JournalRoute:

  protected type OurSession = SimpleSession

  private implicit val timeout: FiniteDuration = 99.s
  private implicit val routeTestTimeout: RouteTestTimeout = RouteTestTimeout(timeout)
  protected def whenShuttingDown = Deferred.unsafe

  private lazy val directory = createTempDirectory("JournalRouteTest-")
  private lazy val journalLocation = JournalLocation(ControllerState, directory / "test")
  override protected def config = config"""
    js7.web.chunk-size = 1MiB
    pekko.loglevel = debug
    """
    .withFallback(JournalEventWatch.TestConfig)
    .withFallback(super.config)
  protected var eventWatch: JournalEventWatch = null
  private val journalId = JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF"))
  private var eventWriter: EventJournalWriter = null

  private lazy val allocatedWebServer = PekkoWebServer
    .testResource()(pathSegments("journal")(journalRoute))
    .toAllocated
    .await(99.s)

  private lazy val uri = allocatedWebServer.allocatedThing.localUri
  private lazy val client = new PekkoHttpClient.Standard(uri, actorSystem = system,
    name = "JournalRouteTest")

  private given IORuntime = ioRuntime

  override def beforeAll() =
    super.beforeAll()
    eventWatch = new JournalEventWatch(journalLocation, config)
    allocatedWebServer
    writeSnapshot(EventId.BeforeFirst)
    eventWriter = newEventJournalWriter(EventId.BeforeFirst)
    eventWriter.onJournalingStarted()

  override def afterAll() =
    try
      allocatedWebServer.release.await(99.s)
      eventWriter.close()
      deleteDirectoryRecursively(directory)
    finally
      super.afterAll()

  implicit private val sessionToken: IO[Option[SessionToken]] = IO.pure(None)
  private lazy val file0 = journalLocation.file(0L)

  "/journal from start" in repeatTest(if isIntelliJIdea then 100_000 else 1000): _ =>
    val lines = client.getRawLinesStream(Uri(s"$uri/journal?timeout=0&file=0&position=0"))
      .flatMap(_.compile.toList).await(99.s)
    assert(lines.map(_.utf8String).mkString == file0.contentString)

  "/journal from end of file" in:
    val fileLength = size(file0)
    val lines = client.getRawLinesStream(Uri(s"$uri/journal?timeout=0&file=0&position=$fileLength"))
      .await(99.s).toListL.await(99.s)
    assert(lines.map(_.utf8String).isEmpty)

  "New data" - {
    val observed = mutable.Buffer[String]()
    var observing: CancelableFuture[Unit] = null

    "Nothing yet written" in:
      val initialFileLength = size(journalLocation.file(0L))
      observing = client
        .getRawLinesStream(Uri(s"$uri/journal?timeout=9&markEOF=true&file=0&position=$initialFileLength"))
        .await(99.s)
        .map(_.utf8String)
        .foreach(string => IO:
          observed += string)
        .compile.drain
        .unsafeToCancelableFuture()
      sleep(100.ms)
      assert(observed.isEmpty)

    "Written but not flushed" in:
      eventWriter.writeEvent(Stamped(1000L, OrderId("1") <-: OrderAdded(WorkflowPath("TEST") ~ "VERSION")))
      sleep(100.ms)
      assert(observed.isEmpty)

    "flushed" in:
      assert(observed.isEmpty)
      eventWriter.flush(false)
      awaitAndAssert(observed.nonEmpty)
      assert(observed.mkString ==
         """{"eventId":1000,"Key":"1","TYPE":"OrderAdded","workflowId":{"path":"TEST","versionId":"VERSION"}}
           |""".stripMargin)

    "committed" in:
      // Journal web service reads uncommitted !!!
      eventWriter.onCommitted(eventWriter.fileLengthAndEventId, n = 1)

    "Next file" in:
      eventWriter.endEventSection(sync = false)
      eventWriter.close()
      observing.await(99.s)
      assert(observed.mkString ==
         """{"eventId":1000,"Key":"1","TYPE":"OrderAdded","workflowId":{"path":"TEST","versionId":"VERSION"}}
           |""".stripMargin ++
           EndOfJournalFileMarker.utf8String)

      writeSnapshot(1000L)
      eventWriter = newEventJournalWriter(1000L)
      eventWriter.onJournalingStarted()
  }

  "Truncated record is ignored" in:
    eventWriter.endEventSection(sync = false)
    eventWriter.close()
    eventWatch.close()

    writeSnapshot(2000L)
    val file2 = journalLocation.file(2000L)
    val file2size = size(file2)
    file2 ++= "{"  // Truncated record

    val file3 = journalLocation.file(3000L)
    writeSnapshot(3000L)

    eventWatch = new JournalEventWatch(journalLocation, config)
    eventWatch.onJournalingStarted(file3, journalId,
      PositionAnd(size(file3), 3000L), PositionAnd(size(file3), 3000L), isActiveNode = true)

    val lines = client.getRawLinesStream(Uri(s"$uri/journal?timeout=0&markEOF=true&file=2000&position=$file2size"))
      .await(99.s).toListL.await(99.s)
    assert(lines == List(EndOfJournalFileMarker))

    eventWatch.close()

  "Acknowledgements" - {
    lazy val file4 = journalLocation.file(4000L)

    "Reading acknowledgements from active node is pointless and rejected" in:
      writeSnapshot(4000L)
      val file4size = size(file4)
      eventWatch = new JournalEventWatch(journalLocation, config)
      eventWatch.onJournalingStarted(file4, journalId, PositionAnd(file4size, 4000L), PositionAnd(file4size, 4000L), isActiveNode = true)
      val bad = HttpClient.liftProblem(client.getRawLinesStream(Uri(
        s"$uri/journal?timeout=0&markEOF=true&file=4000&position=$file4size&return=ack")))
      assert(bad.await(99.s) == Left(AckFromActiveClusterNodeProblem))

      eventWatch.close()

    "Reading acknowledgements from passive node is good" in:
      val file4size = size(file4)
      eventWatch = new JournalEventWatch(journalLocation, config)
      eventWatch.onJournalingStarted(file4, journalId, PositionAnd(file4size, 4000L), PositionAnd(file4size, 4000L), isActiveNode = false)
      val lines = client.getRawLinesStream(Uri(s"$uri/journal?timeout=0&markEOF=true&file=4000&position=$file4size&return=ack"))
        .await(99.s).toListL.await(99.s)
      assert(lines == Nil)

      eventWatch.close()
  }

  private def writeSnapshot(eventId: EventId): Unit =
    autoClosing(newSnapshotJournalWriter(eventId)) { writer =>
      writer.writeHeader(JournalHeaders.forTest("TestState", journalId))
      writer.beginSnapshotSection()
      writer.endSnapshotSection()
      writer.beginEventSection(sync = false)
      writer.writeEvent(Stamped(eventId + 1, NoKey <-: SnapshotTaken))
    }

  private def newSnapshotJournalWriter(eventId: EventId) =
    new SnapshotJournalWriter(journalLocation.S, journalLocation.file(eventId), after = eventId, simulateSync = None)

  private def newEventJournalWriter(eventId: EventId) =
    new EventJournalWriter(journalLocation.S, journalLocation.file(eventId), after = eventId, journalId, Some(eventWatch), simulateSync = None)
