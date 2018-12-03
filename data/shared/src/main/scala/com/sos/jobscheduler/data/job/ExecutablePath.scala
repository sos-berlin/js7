package com.sos.jobscheduler.data.job

import cats.data.Validated.Invalid
import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.problem.Problem
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final case class ExecutablePath private(string: String) extends GenericString
{
  def toFile(directory: Path): Path =  // TODO Check for ".."
    directory resolve string.stripPrefix("/")
}

object ExecutablePath extends GenericString.Checked_[ExecutablePath]
{
  protected def unchecked(string: String) = new ExecutablePath(string)

  def sh(string: String) = apply(if (sys.props("os.name") startsWith "Windows") s"$string.cmd" else string)

  override def checked(string: String) =
    if (string.isEmpty)
      Invalid(Problem("Executable path must not be empty"))
    else if (!string.startsWith("/") || string == "/" || string.contains('\\') || string.startsWith(".") || string.contains("/."))
      Invalid(Problem(s"Invalid executable path: $string"))
    else
      super.checked(string)
}
