package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.generic.IsString

/**
  * @author Joacim Zschimmer
  */
final case class Label(string: String) extends IsString {
  //scala.js Identifier.requireIdentifier(string)
}

object Label extends IsString.Companion[Label]
