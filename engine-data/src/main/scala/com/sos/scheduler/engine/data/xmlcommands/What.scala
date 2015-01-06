package com.sos.scheduler.engine.data.xmlcommands

import com.sos.scheduler.engine.data.base.IsString

sealed trait What extends IsString

abstract class AbstractWhat(val string: String) extends What

object What {
  case object Log extends AbstractWhat("log")
}
