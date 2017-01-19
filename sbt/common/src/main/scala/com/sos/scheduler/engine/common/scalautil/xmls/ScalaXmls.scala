package com.sos.scheduler.engine.common.scalautil.xmls

import akka.util.ByteString
import com.google.common.base.Charsets.UTF_8
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import java.io.File
import java.nio.charset.Charset
import java.nio.file.Path

object ScalaXmls {

  object implicits {

    implicit class RichXmlFile(val delegate: File) extends AnyVal {

      def xml = SafeXML.loadFile(delegate)

      def xml_=(o: scala.xml.Elem): Unit = {
        scala.xml.XML.save(delegate.getPath, o, enc = UTF_8.name, xmlDecl = true)
      }
    }

    implicit class RichXmlPath(val delegate: Path) extends AnyVal {

      def xml = SafeXML.loadFile(delegate)

      def xml_=(o: scala.xml.Elem): Unit = {
        scala.xml.XML.save(delegate.getPath, o, enc = UTF_8.name, xmlDecl = true)
      }
    }

    implicit class RichElem(val delegate: xml.Elem) extends AnyVal {
      def toByteString: ByteString = toByteString(xmlDecl = true)

      def toByteString(encoding: Charset = UTF_8, xmlDecl: Boolean = true): ByteString = {
        val b = new StringBuilder
        if (xmlDecl) {
          b.append(s"<?xml version='1.0' encoding='${ encoding.name }'?>")
        }
        xml.Utility.serialize(delegate, sb = b)
        ByteString(b.toString().getBytes(encoding))
      }
    }
  }
}
