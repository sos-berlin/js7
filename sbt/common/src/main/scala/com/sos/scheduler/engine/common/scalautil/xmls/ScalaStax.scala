package com.sos.scheduler.engine.common.scalautil.xmls

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.scalautil.ScalaThreadLocal._
import java.util.concurrent.atomic.AtomicBoolean
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.events.{Attribute, StartElement}
import scala.collection.JavaConversions._

object ScalaStax {
  private val logger = Logger(getClass)

  implicit final class RichStartElement(val delegate: StartElement) extends AnyVal {
    def attributes: Iterator[Attribute] =
      delegate.getAttributes.asInstanceOf[java.util.Iterator[Attribute]]
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
   * Fast XMLInputFactory provider - returned XMLInputFactory is mutable, do not change it.
   */
  private[common] def getCommonXMLInputFactory(): XMLInputFactory =
    xmlInputFactoryTL
}
