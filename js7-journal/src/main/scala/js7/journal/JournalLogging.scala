package js7.journal

import js7.journal.JournalActor.Persist
import js7.journal.configuration.JournalConf
import js7.journal.log.JournalLogger
import scala.collection.IndexedSeqView

trait JournalLogging
{
  protected val conf: JournalConf

  private lazy val journalLogger = new JournalLogger(
    syncOrFlushChars =
      if (!conf.syncOnCommit)
        "flush"
      else if (conf.simulateSync.isDefined)
        "~sync"
      else
        "sync ",
    infoLogEvents = conf.infoLogEvents)

  protected final def logCommitted(persists: IndexedSeqView[Persist], ack: Boolean) =
    journalLogger.logCommitted(persists, ack)
}
