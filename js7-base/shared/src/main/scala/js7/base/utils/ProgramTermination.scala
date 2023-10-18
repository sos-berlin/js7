package js7.base.utils

import js7.base.utils.ScalaUtils.syntax.*

trait ProgramTermination:
  def restart: Boolean
  override def toString = s"ProgramTermination(${restart ?? "restart"})"


object ProgramTermination:
  def apply(restart: Boolean = false): ProgramTermination =
    Standard(restart)

  def unapply(termination: ProgramTermination): Option[Boolean] =
    Some(termination.restart)

  private final case class Standard(restart: Boolean) extends ProgramTermination:
    override def toString = s"ProgramTermination(${restart ?? "restart"})"
