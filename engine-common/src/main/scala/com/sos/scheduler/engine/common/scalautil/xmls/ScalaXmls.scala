package com.sos.scheduler.engine.common.scalautil.xmls

import com.google.common.base.Charsets.UTF_8
import java.io.File
import java.nio.charset.Charset

object ScalaXmls {

  object implicits {

    implicit class RichXmlFile(val delegate: File) extends AnyVal {

      def xml = SafeXML.loadFile(delegate)

      def xml_=(o: scala.xml.Elem): Unit = {
        scala.xml.XML.save(delegate.getPath, o, enc = UTF_8.name, xmlDecl = true)
      }
    }

    implicit class RichElem(val delegate: xml.Elem) extends AnyVal {
      def toBytes: Array[Byte] = toBytes(xmlDecl = true)

      def toBytes(encoding: Charset = UTF_8, xmlDecl: Boolean = true): Array[Byte] = {
        val b = new StringBuilder
        if (xmlDecl) {
          b.append(s"<?xml version='1.0' encoding='${ encoding.name }'?>")
        }
        xml.Utility.serialize(delegate, sb = b)
        b.toString().getBytes(encoding)
      }
    }
  }
}
