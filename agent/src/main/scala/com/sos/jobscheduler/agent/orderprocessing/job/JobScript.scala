package com.sos.scheduler.engine.agent.orderprocessing.job

import com.sos.scheduler.engine.base.generic.IsString

/**
  * @author Joacim Zschimmer
  */
final case class JobScript(string: String) extends IsString {

  def toXmlElem: xml.Elem =
    <source><source_part linenr="1">{string}</source_part></source>
}
