package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.problem.{Problem, ProblemException}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final class ClosedException private[watch](file: Path)
extends ProblemException(Problem.pure(s"Journal file '${file.getFileName}' cannot be read because EventReader has been closed"))
