package com.sos.jobscheduler.agent.scheduler.job

import com.sos.jobscheduler.base.generic.GenericString

/**
  * @author Joacim Zschimmer
  */
final case class JobScript(string: String) extends GenericString {

  def toXmlElem: xml.Elem =
    <source><source_part linenr="1">{string}</source_part></source>
}
