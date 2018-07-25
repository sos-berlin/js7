package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.problem.ProblemException
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final class ClosedException private[watch](file: Path)
extends ProblemException(s"Journal cannot be read because GenericJournalEventReader has been closed: '$file'")
