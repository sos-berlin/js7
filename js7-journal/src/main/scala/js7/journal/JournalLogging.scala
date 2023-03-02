package js7.journal

import js7.journal.configuration.JournalConf
import js7.journal.log.JournalLogger

trait JournalLogging
{
  protected val conf: JournalConf

  protected final val journalLogger = new JournalLogger(
    syncOrFlushChars =
      if (!conf.syncOnCommit)
        "flush"
      else if (conf.simulateSync.isDefined)
        "~sync"
      else
        "sync ",
    infoLogEvents = conf.infoLogEvents)
}
