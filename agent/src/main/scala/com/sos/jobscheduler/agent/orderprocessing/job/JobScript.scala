package com.sos.jobscheduler.agent.orderprocessing.job

import com.sos.jobscheduler.base.generic.IsString

/**
  * @author Joacim Zschimmer
  */
final case class JobScript(string: String) extends IsString {

  def toXmlElem: xml.Elem =
    <source><source_part linenr="1">{string}</source_part></source>
}
