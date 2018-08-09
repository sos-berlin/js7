package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.problem.ProblemException

/**
  * @author Joacim Zschimmer
  */
final class ClosedException private[watch]
extends ProblemException(s"Journal cannot be read because EventReader has been closed'")
