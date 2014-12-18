package com.sos.scheduler.engine.common.scalautil.xmls

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.scalautil.ScalaThreadLocal._
import com.sos.scheduler.engine.common.xml.XmlUtils.toXmlBytes
import java.io.ByteArrayInputStream
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.events.{Attribute, StartElement}
import javax.xml.transform.Source
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamSource
import org.w3c.dom.Element
import scala.collection.JavaConversions._
import scala.util.control.NonFatal

object ScalaStax {
  private val logger = Logger(getClass)

  implicit final class RichStartElement(val delegate: StartElement) extends AnyVal {
    def attributes: Iterator[Attribute] =
      delegate.getAttributes.asInstanceOf[java.util.Iterator[Attribute]]
  }

  def domElementToStaxSource(e: org.w3c.dom.Element): Source = DomElementToStaxSource(e)

  private object DomElementToStaxSource {
    @volatile private var factory: Option[Element ⇒ Source] = None

    def apply(element: Element): Source =
      factory match {
        case Some(f) ⇒ f(element)
        case None ⇒ determineFactory(element)
      }

    def determineFactory(element: Element) =
      synchronized {
        factory = Some(newDomSource)
        try {
          val result = newDomSource(element)
          getCommonXMLInputFactory().createXMLEventReader(result)
          result
        }
        catch {
          case t: java.lang.UnsupportedOperationException ⇒ // "Cannot create XMLStreamReader or XMLEventReader from a javax.xml.transform.dom.DOMSource"
            logger.debug(s"new DOMSource(org.w3c.dom.Element) => $t - working around this")
            try {
              val result = workAroundNewDomSource(element)
              factory = Some(workAroundNewDomSource)
              result
            }
            catch {
              case NonFatal(tt) ⇒
                t.addSuppressed(tt)
                throw t;
            }
        }
      }

    private def newDomSource(element: Element) = new DOMSource(element)

    private def workAroundNewDomSource(element: Element) = new StreamSource(new ByteArrayInputStream(toXmlBytes(element)))
  }

  private val xmlInputFactoryTL = threadLocal { XMLInputFactory.newInstance() }

  /**
   * Fast XMLInputFactory provider - returned XMLInputFactory is mutable, do not change it.
   */
  def getCommonXMLInputFactory(): XMLInputFactory = xmlInputFactoryTL
}
