package com.sos.scheduler.engine.common.scalautil.xmls

object RichScalaXML {
  implicit class RichElem(val delegate: xml.Node) extends AnyVal {

    /** @return String des Attributs oder "" */
    def attributeText(name: String): String =
      delegate.attribute(name) map { _.text } getOrElse ""
  }
}
