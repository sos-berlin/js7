package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.generic.GenericString
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class Label(string: String) extends GenericString {
  //scala.js Identifier.requireIdentifier(string)  TODO Syntax für Label?
}

object Label extends GenericString.Companion[Label] {
  implicit def fromString(label: String) = new Label(label)
}
