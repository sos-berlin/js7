package js7.base.utils

import js7.base.utils.ScalaUtils.syntax.*

final case class ProgramTermination(restart: Boolean = false)
{
  override def toString = s"ProgramTermination(${restart ?? "restart"})"
}
