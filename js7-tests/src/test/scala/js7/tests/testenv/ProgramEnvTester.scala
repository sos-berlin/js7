package js7.tests.testenv

import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.time.WaitForCondition.waitForCondition
import js7.journal.files.JournalFiles.listJournalFiles
import org.scalactic.source
import org.scalatest.Assertions.*

object ProgramEnvTester {

  def assertEqualJournalFiles(primary: ProgramEnv, backup: ProgramEnv, n: Int)
    (implicit pos: source.Position)
  : Unit = {
    waitForCondition(9.s, 10.ms) { listJournalFiles(primary.journalFileBase).size == n }
    val journalFiles = listJournalFiles(primary.journalFileBase)
    // Snapshot is not being acknowledged, so a new journal file starts asynchronously (or when one event has been written)
    assert(journalFiles.size == n)
    waitForCondition(9.s, 10.ms) { listJournalFiles(backup.journalFileBase).size == n }
    for (primaryFile <- journalFiles.map(_.file)) {
      withClue(s"$primaryFile: ") {
        val backupJournalFile = backup.stateDir.resolve(primaryFile.getFileName)
        waitForCondition(9.s, 100.ms)(backupJournalFile.contentString == primaryFile.contentString)
        assert(backupJournalFile.contentString == primaryFile.contentString)
        assert(backupJournalFile.byteArray == primaryFile.byteArray)
      }
    }
  }
}
