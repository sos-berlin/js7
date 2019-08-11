package com.sos.jobscheduler.data.job

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.generic.GenericString.EmptyStringProblem
import com.sos.jobscheduler.base.problem.Problems.InvalidNameProblem
import java.nio.file.Path

sealed trait Executable

final case class ExecutablePath private(path: String)
extends Executable with GenericString
{
  def string = path

  def toFile(directory: Path): Path =
    directory resolve path.stripPrefix("/")
}

object ExecutablePath extends GenericString.Checked_[ExecutablePath]
{
  protected def unchecked(path: String) = new ExecutablePath(path)

  def sh(path: String) = apply(if (sys.props("os.name") startsWith "Windows") s"$path.cmd" else path)

  override def checked(path: String) =
    if (path.isEmpty)
      Left(EmptyStringProblem(name))
    else if (!path.startsWith("/") || path == "/" || path.contains('\\') || path.startsWith(".") || path.contains("/."))  // TODO Check for ".."
      Left(InvalidNameProblem(name, path))
    else
      super.checked(path)
}

final case class ExecutableScript(script: String)
extends Executable with GenericString
{
  def string = script
}

object ExecutableScript extends GenericString.Companion[ExecutableScript]

object Executable
{
  implicit val jsonCodec = TypedJsonCodec[Executable](
    Subtype(deriveCodec[ExecutablePath]),
    Subtype(deriveCodec[ExecutableScript]))
}
