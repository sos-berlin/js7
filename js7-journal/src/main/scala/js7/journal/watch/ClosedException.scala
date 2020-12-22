package js7.journal.watch

import java.nio.file.Path
import js7.base.problem.{Problem, ProblemException}

/**
  * @author Joacim Zschimmer
  */
final class ClosedException private[watch](file: Path)
extends ProblemException(Problem.pure(s"Journal file '${file.getFileName}' cannot be read because EventReader has been closed"))
