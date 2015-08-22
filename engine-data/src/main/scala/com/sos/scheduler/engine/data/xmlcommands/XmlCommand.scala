package com.sos.scheduler.engine.data.xmlcommands

import scala.language.implicitConversions

trait XmlCommand {
  def xmlElem: xml.Elem
  def xmlString: String = xmlElem.toString()
}

object XmlCommand {
  implicit def implicitToXmlElem(o: XmlCommand): xml.Elem = o.xmlElem
}
