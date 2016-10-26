package com.sos.scheduler.engine.common.xml

import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichTraversableOnce
import org.w3c.dom.{Element, Node, NodeList}
import scala.collection.immutable

/**
  * @author Joacim Zschimmer
  */
object DomForScala {

  implicit class SeqNodeList(val delegate: NodeList) extends AnyVal {
    def toSeq: IndexedSeq[Node] = new IndexedSeq[Node] {
      def length = delegate.getLength
      def apply(i: Int) = delegate.item(i)
    }
  }

  implicit class RichElement(val delegate: Element) extends AnyVal {
    def childNodes: IndexedSeq[Node] =
      delegate.getChildNodes.toSeq

    def childElements: immutable.Seq[Element] =
      delegate.getChildNodes.toSeq.toVector collect { case e: Element ⇒ e }

    def childElementsByName(name: String): immutable.Seq[Element] =
      delegate.getChildNodes.toSeq.toVector collect { case e: Element if e.getTagName == name ⇒ e }

    def /(name: String): Seq[Element] =
      childElementsByName(name)
  }

  implicit class RichElements(val delegate: TraversableOnce[Element]) extends AnyVal {
    def /(name: String): immutable.Seq[Element] =
      for (element ← delegate.toImmutableSeq; child ← element / name) yield child
  }
}
