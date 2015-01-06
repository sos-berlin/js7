package com.sos.scheduler.engine.data.xmlcommands

trait XmlCommand {
  def xmlElem: xml.Elem
  def xmlString: String = xmlElem.toString()
}
