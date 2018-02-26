package com.sos.jobscheduler.common.scalautil.xmls

object RichScalaXML {
  implicit final class RichElem(private val delegate: xml.Node) extends AnyVal {

    /** @return String des Attributs oder "" */
    def attributeText(name: String): String =
      delegate.attribute(name) map { _.text } getOrElse ""
  }
}
