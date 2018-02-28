package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.generic.IsString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}

/**
  * @author Joacim Zschimmer
  */
final case class FileBasedVersion(string: String) extends IsString {
  FileBasedVersion.check(string).force

  override def toString = s"version $string"
}

object FileBasedVersion extends IsString.Companion[FileBasedVersion]
{
  def checked(string: String): Checked[FileBasedVersion] =
    check(string) map (_ ⇒ apply(string))

  def check(string: String): Checked[Unit] =
    if (string.isEmpty)
      Problem("Empty version identifier?")
    else
      Checked.unit
}
