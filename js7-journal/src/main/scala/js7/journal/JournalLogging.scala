package js7.journal

import js7.journal.configuration.JournalConf
import js7.journal.log.JournalLogger

trait JournalLogging:
  protected val conf: JournalConf

  protected final val journalLogger = new JournalLogger(
    syncOrFlushString =
      if !conf.syncOnCommit then
        "flush"
      else if conf.simulateSync.isDefined then
        "~sync"
      else
        "sync ",
    infoLogEvents = conf.infoLogEvents)
