package js7.journal.recover

import cats.effect.unsafe.IORuntime
import io.circe.Encoder
import io.circe.syntax.EncoderOps
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.delete
import java.util.UUID
import js7.base.circeutils.CirceUtils.{RichCirceString, RichJson}
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.data.event.JournalEvent.SnapshotTaken
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournalEvent, JournalHeader, JournalId, JournalSeparators, KeyedEvent, Stamped}
import js7.journal.JournalActor
import js7.journal.files.JournalFiles.extensions.*
import js7.journal.test.{TestActor, TestAggregate, TestAggregateActor, TestEvent, TestJournalMixin}
import js7.journal.watch.JournalingObserver
import js7.journal.write.{EventJournalWriter, FileJsonWriter, SnapshotJournalWriter}
import org.apache.pekko.pattern.ask

/**
  * @author Joacim Zschimmer
  */
final class JournalReaderTest extends OurTestSuite, TestJournalMixin:

  private given IORuntime = ioRuntime

  private val journalId = JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF"))
  private val stateName = "TestState"

  "Journal file without snapshots or events" in:
    withTestActor() { (_, _) => }
    val file = currentFile
    val journalId =
      autoClosing(scala.io.Source.fromFile(file.toFile)(UTF_8))(_.getLines().next())
        .parseJsonAs[JournalHeader].orThrow.journalId
    autoClosing(JournalReader(journalLocation.S, file, journalId)): journalReader =>
      assert(journalReader.readSnapshot.compile.toList.await(99.s) == journalReader.journalHeader :: Nil)
      assert(journalReader.readEvents().toList == Stamped(1000000L, (NoKey <-: JournalEvent.SnapshotTaken)) :: Nil)
      assert(journalReader.eventId == 1000000)
      assert(journalReader.totalEventCount == 1)

  "Journal file with snapshot section only" in:
    val file = currentFile
    delete(file)  // File of last test
    autoClosing(
      SnapshotJournalWriter(journalLocation.S, file, after = EventId.BeforeFirst, simulateSync = None)
    ): writer =>
      writer.writeHeader(JournalHeader.forTest(stateName, journalId))
      writer.beginSnapshotSection()
      writer.endSnapshotSection()
      writer.beginEventSection(sync = false)
      writer.writeEvent(Stamped(1000L, NoKey <-: SnapshotTaken))

    autoClosing(
      JournalReader(journalLocation.S, journalLocation.currentFile.orThrow, journalId)
    ): journalReader =>
      assert(journalReader.readSnapshot.compile.toList.await(99.s) == journalReader.journalHeader :: Nil)
      assert(journalReader.readEvents().toList == Stamped(1000L, (NoKey <-: JournalEvent.SnapshotTaken)) :: Nil)
      assert(journalReader.eventId == 1000)
      assert(journalReader.totalEventCount == 1)

  "Journal file with open event section" in:
    val file = currentFile
    delete(file)  // File of last test
    autoClosing(
      SnapshotJournalWriter(journalLocation.S, file, after = EventId.BeforeFirst, simulateSync = None)
    ): writer =>
      writer.writeHeader(JournalHeader.forTest(stateName, journalId))
      writer.beginSnapshotSection()
      writer.endSnapshotSection()
      writer.beginEventSection(sync = false)
      writer.writeEvent(Stamped(1000L, NoKey <-: SnapshotTaken))

    autoClosing(
      EventJournalWriter(journalLocation, after = EventId.BeforeFirst, journalId,
        observer = JournalingObserver.Dummy, simulateSync = None)
    ): writer =>
      writer.writeEvents(Stamped(1001L, "X" <-: TestEvent.Removed) :: Nil)
      //Without: writer.endEventSection(sync = false)

    autoClosing(
      JournalReader(journalLocation.S, journalLocation.currentFile.orThrow, journalId)
    ): journalReader =>
      assert(journalReader.fileEventId == 0)
      assert(journalReader.eventId == EventId.BeforeFirst)
      assert(journalReader.readSnapshot.compile.toList.await(99.s) == journalReader.journalHeader :: Nil)
      assert(journalReader.readEvents().toList == Stamped(1000L, NoKey <-: SnapshotTaken) :: Stamped(1001L, "X" <-: TestEvent.Removed) :: Nil)
      assert(journalReader.eventId == 1001)
      assert(journalReader.totalEventCount == 2)

  "Journal file with snapshot and events" in:
    withTestActor() { (actorSystem, actor) =>
      for (key, cmd) <- testCommands("TEST") do execute(actorSystem, actor, key, cmd).await(99.s)
      (actor ? TestActor.Input.TakeSnapshot).mapTo[JournalActor.Output.SnapshotTaken.type].await(99.s)
      execute(actorSystem, actor, "X", TestAggregateActor.Command.Add("(X)")).await(99.s)
      execute(actorSystem, actor, "Y", TestAggregateActor.Command.Add("(Y)")).await(99.s)
    }
    autoClosing(JournalReader(journalLocation.S, currentFile, journalId)): journalReader =>
      assert(journalReader.readSnapshot.compile.to(Set).await(99.s) == Set(
        journalReader.journalHeader,
        TestAggregate("TEST-A","(A.Add)(A.Append)(A.AppendAsync)(A.AppendNested)(A.AppendNestedAsync)"),
        TestAggregate("TEST-C","(C.Add)")))
      assert(journalReader.readEvents().toList == List(
        Stamped(1000067L, NoKey <-: SnapshotTaken),
        Stamped(1000068L, "X" <-: TestEvent.Added("(X)")),
        Stamped(1000069L, "Y" <-: TestEvent.Added("(Y)"))))
      assert(journalReader.eventId == 1000069)
      assert(journalReader.totalEventCount == 72)

  "Transactions" - {
    val first = Stamped(1000L, "A" <-: TestEvent.Removed)
    val ta = Stamped(1001L, "KEY" <-: TestEvent.Appended('a')) ::
             Stamped(1002L, "KEY" <-: TestEvent.Appended('b')) ::
             Stamped(1003L, "KEY" <-: TestEvent.Appended('c')) :: Nil
    val last = Stamped(1004L, "Z" <-: TestEvent.Removed)

    "Committed transaction" in:
      val file = journalLocation.file(0L)
      delete(file)  // File of last test
      autoClosing(
        EventJournalWriter(journalLocation, after = 0L, journalId,
          observer = JournalingObserver.Dummy, simulateSync = None, append = false)
      ): writer =>
        writer.writeHeader(JournalHeader.forTest(stateName, journalId, eventId = EventId.BeforeFirst))
        writer.beginEventSection(sync = false)
        writer.writeEvents(first :: Nil)
        writer.writeEvents(ta, transaction = true)
        writer.writeEvents(last :: Nil)
        writer.endEventSection(sync = false)

      autoClosing(
        JournalReader(journalLocation.S, file, journalId)
      ): journalReader =>
        assert(journalReader.readEvents().toList == first :: ta ::: last :: Nil)
        assert(journalReader.eventId == 1004)
        assert(journalReader.totalEventCount == 5)

      autoClosing(
        JournalReader(journalLocation.S, file, journalId)
      ): journalReader =>
        assert(journalReader.nextEvent() == Some(first))
        assert(journalReader.eventId == 1000)
        assert(journalReader.nextEvent() == Some(ta(0)))
        assert(journalReader.eventId == 1001)
        assert(journalReader.nextEvent() == Some(ta(1)))
        assert(journalReader.eventId == 1002)
        assert(journalReader.nextEvent() == Some(ta(2)))
        assert(journalReader.eventId == 1003)
        assert(journalReader.nextEvent() == Some(last))
        assert(journalReader.eventId == 1004)
        assert(journalReader.nextEvent() == None)
        assert(journalReader.eventId == 1004)
        assert(journalReader.totalEventCount == 5)

    "Uncommitted transaction" in:
      val file = currentFile
      delete(file)  // File of last test
      autoClosing(FileJsonWriter(file)): writer =>
        def write[A](a: A)(using encoder: Encoder[A]) = writer.write(encoder(a).toByteArray)
        def writeEvent(stamped: Stamped[KeyedEvent[TestEvent]]) = write(stamped)

        write(JournalHeader.forTest(stateName, journalId, eventId = EventId.BeforeFirst).asJson)
        write(JournalSeparators.EventHeader)
        writeEvent(first)
        write(JournalSeparators.Transaction)
        writeEvent(ta(0))
        writeEvent(ta(1))
        writeEvent(ta(2))

      autoClosing(
        JournalReader(journalLocation.S, journalLocation.currentFile.orThrow, journalId)
      ): journalReader =>
        assert(journalReader.readEvents().toList == first :: Nil)  // Uncommitted transaction is not read
        assert(journalReader.eventId == 1000)
        assert(journalReader.totalEventCount == 1)
  }

  private def currentFile = journalLocation.currentFile.orThrow
