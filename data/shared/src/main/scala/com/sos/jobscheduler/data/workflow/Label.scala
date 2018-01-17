package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.generic.IsString
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class Label(string: String) extends IsString {
  //scala.js Identifier.requireIdentifier(string)  TODO Syntax für Label?
}

object Label extends IsString.Companion[Label] {
  implicit def fromString(label: String) = new Label(label)
}
