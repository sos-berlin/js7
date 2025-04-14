package js7.journal

import js7.journal.configuration.JournalConf
import js7.journal.log.JournalLogger

trait JournalLogging:
  protected val conf: JournalConf

  protected final lazy val journalLogger = JournalLogger(conf)
