package com.sos.jobscheduler.common.scalautil.xmls

import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.ScalaThreadLocal._
import java.util.concurrent.atomic.AtomicBoolean
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.events.{Attribute, StartElement}
import scala.jdk.CollectionConverters._

object ScalaStax {
  private val logger = Logger(getClass)

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
  private[sos] def getCommonXMLInputFactory(): XMLInputFactory =
    xmlInputFactoryTL
}
