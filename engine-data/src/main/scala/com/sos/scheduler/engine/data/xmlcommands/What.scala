package com.sos.scheduler.engine.data.xmlcommands

import com.sos.scheduler.engine.base.generic.IsString

sealed trait What extends IsString

abstract class AbstractWhat(val string: String) extends What

object What {
  case object Log extends AbstractWhat("log")
}
