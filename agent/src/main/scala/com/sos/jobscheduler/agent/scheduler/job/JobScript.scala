package js7.agent.scheduler.job

import js7.base.generic.GenericString

/**
  * @author Joacim Zschimmer
  */
final case class JobScript(string: String) extends GenericString {

  def toXmlElem: xml.Elem =
    <source><source_part linenr="1">{string}</source_part></source>
}
