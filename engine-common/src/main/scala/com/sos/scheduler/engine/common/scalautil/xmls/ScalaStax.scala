package com.sos.scheduler.engine.common.scalautil.xmls

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.scalautil.ScalaThreadLocal._
import com.sos.scheduler.engine.common.xml.XmlUtils.toXmlBytes
import java.io.{ByteArrayInputStream, StringReader}
import java.util.concurrent.atomic.AtomicBoolean
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

  def xmlElemToStaxSource(elem: xml.Elem): Source = new StreamSource(new StringReader(elem.toString()))

  def domElementToStaxSource(e: org.w3c.dom.Element): Source = DomElementToStaxSource(e)

  private object DomElementToStaxSource {
    @volatile private var factory: Option[Element ⇒ Source] = None

    def apply(element: Element): Source =
      factory match {
        case Some(f) ⇒ f(element)
        case None ⇒ workAroundNewDomSource(element)  //determineFactory(element)
      }

    // Unused. Obviously, there is no direct conversion org.w3c.dom.Element => DOMSource. And if so, we should test it in development.
    private def determineFactory(element: Element) =
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

  private val xmlInputFactoryLogged = new AtomicBoolean
  private val xmlInputFactoryTL = threadLocal {
    val result = XMLInputFactory.newInstance()
    if (!xmlInputFactoryLogged.getAndSet(true)) {
      logger.info(s"Using XMLInputFactory ${result.getClass}")
    }
    result
  }

  /**
   * Fast XMLInputFactory provider - returned XMLInputFactory is mutable, do not change it.
   */
  private[common] def getCommonXMLInputFactory(): XMLInputFactory =
    xmlInputFactoryTL
}
