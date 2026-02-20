package js7.data.cluster

import js7.base.generic.GenericString

final case class Confirmer(string: String) extends GenericString:
  override def toString = s"Confimer:$string"


object Confirmer extends GenericString.Checked_[Confirmer]:
  protected def unchecked(string: String) = new Confirmer(string)
