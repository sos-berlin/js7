package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.generic.GenericString
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class Label private(string: String) extends GenericString {
  //scala.js Identifier.requireIdentifier(string)  TODO Syntax für Label?
}

object Label extends GenericString.Checked_[Label]
{
  def unchecked(string: String) = new Label(string)

  implicit def fromString(label: String): Label = super.apply(label)
}
