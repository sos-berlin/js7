package js7.common.scalautil.xmls

import java.util.concurrent.atomic.AtomicBoolean
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.events.{Attribute, StartElement}
import js7.base.log.Logger
import js7.common.scalautil.ScalaThreadLocal.*
import scala.jdk.CollectionConverters.*

object ScalaStax {
  private val logger = Logger[this.type]

  implicit final class RichStartElement(private val delegate: StartElement) extends AnyVal {
    def attributes: Iterator[Attribute] =
      delegate.getAttributes.asInstanceOf[java.util.Iterator[Attribute]].asScala
  }

  private val xmlInputFactoryLogged = new AtomicBoolean
  private val xmlInputFactoryTL = threadLocal {
    val result = XMLInputFactory.newInstance()
    if (!xmlInputFactoryLogged.getAndSet(true)) {
      logger.debug(s"Using XMLInputFactory ${result.getClass}")
    }
    result
  }

  /**
    * Fast XMLInputFactory provider - returned XMLInputFactory is mutable, DO NOT CHANGE IT.
    */
  def getCommonXMLInputFactory(): XMLInputFactory =
    xmlInputFactoryTL
}
