package com.sos.scheduler.engine.common.scalautil.xmls

import com.sos.scheduler.engine.common.scalautil.AssignableFrom.assignableFrom
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.{cast, implicitClass}
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaStax.{RichStartElement, getCommonXMLInputFactory}
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader._
import java.util.NoSuchElementException
import javax.xml.stream.events.{Characters, Comment, EndDocument, EndElement, StartDocument, StartElement, XMLEvent}
import javax.xml.stream.{EventFilter, Location, XMLEventReader, XMLInputFactory}
import javax.xml.transform.Source
import org.scalactic.Requirements._
import scala.PartialFunction._
import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.reflect.ClassTag
import scala.util.control.NonFatal

final class ScalaXMLEventReader(delegate: XMLEventReader) extends AutoCloseable {

  private var atStart = true
  private var _simpleAttributeMap: SimpleAttributeMap = null

  updateAttributeMap()

  def close() = delegate.close()

  def parseDocument[A](body: ⇒ A): A = {
    eat[StartDocument]
    val result = body
    eat[EndDocument]
    result
  }

  def ignoreElement(): Unit = {
    parseElement() {
      attributeMap.ignoreUnread()
      while(!peek.isEndElement) {
        if (peek.isCharacters) eatText()
        else ignoreElement()
      }
    }
  }

  def parseElement[A](name: String)(body: ⇒ A): A = {
    requireStartElement(name)
    parseElement()(body)
  }

  def parseElement[A]()(body: ⇒ A): A =
    wrapException {
      eat[StartElement]
      parseStartElement()(body)
    }

  def parseStartElement[A]()(body: ⇒ A): A = {
    val result = body
    eat[EndElement]
    result
  }

  @deprecated("Use attribute", "1.8")
  def forEachAttribute(f: PartialFunction[(String, String), Unit]): Unit = {
    def callF(nameValue: (String, String)) = {
      try f.applyOrElse(nameValue, { o: (String, String) ⇒ sys.error(s"Unexpected XML attribute ${o._1}") })
      catch {
        case x: Exception ⇒
          val (name, value) = nameValue
          throw new RuntimeException(s"Unparsable XML attribute $name='$value': $x", x)
      }
      attributeMap.get(nameValue._1)  // Mark as read
    }

    attributeMap foreach callF
  }

  def parseEachRepeatingElement[A](name: String)(f: ⇒ A): immutable.Seq[A] =
    forEachStartElement[A] { case `name` ⇒ parseElement() { f } }.values

  def forEachStartElement[A](f: PartialFunction[String, A]): ConvertedElementMap[A] = {
    val results = mutable.Buffer[(String, A)]()

    @tailrec
    def g(): Unit = {
      peek match {
        case e: StartElement ⇒
          val o = parseStartElementAlternative(f)
          results += e.getName.toString -> o
          g()
        case e: EndElement ⇒ mutable.Buffer[(String, A)]()
      }
    }

    g()
    new ConvertedElementMap(results.toVector)
  }

  def parseStartElementAlternative[A](f: PartialFunction[String, A]): A = {
    val name = peek.asStartElement.getName.toString
    f.applyOrElse(name, { name: String ⇒ sys.error(s"Unexpected XML element <$name>") })
  }

  private def wrapException[A](f: ⇒ A): A = {
    val element = peek.asStartElement()
    try f
    catch {
      case x: Exception ⇒ throw new XmlException(element.getName.toString, element.getLocation, x)
    }
  }

  def eatStartElement(name: String) = {
    val e = eat[StartElement]
    require(e.getName.getLocalPart == name, s"XML element <$name> expected instead of <${e.getName}>")
    e
  }

//  import javax.xml.transform.{Result, TransformerFactory}
//  import com.sos.scheduler.engine.common.scalautil.StringWriters.writingString
//  import javax.xml.transform.stax.StAXSource
//  import javax.xml.transform.stream.StreamResult
//  private lazy val transformerFactory = TransformerFactory.newInstance()
//  private[xml] def parseElementAsXmlString(): String =
//    writingString { writer ⇒
//      parseElementInto(new StreamResult(writer))
//    }
//
//  private def parseElementInto(result: Result): Unit =
//  Liest bis zum Stream-Ende statt nur bis zum Endetag:  transformerFactory.newTransformer().transform(new StAXSource(xmlEventReader), result)

  def requireStartElement(name: String) = require(peek.asStartElement.getName.getLocalPart == name)

  def eatText() = {
    val result = new StringBuilder
    while (peek.isCharacters)
      result append eat[Characters].getData
    result.toString()
  }

  def eat[E <: XMLEvent : ClassTag]: E = cast[E](next())

  def hasNext = delegate.hasNext

  def next(): XMLEvent = {
    if (!atStart) {
      updateAttributeMap()
    }
    atStart = false
    delegate.nextEvent()
  }

  private def updateAttributeMap(): Unit = {
    if (_simpleAttributeMap != null) {
      _simpleAttributeMap.requireAllAttributesRead()
    }
    _simpleAttributeMap =
      if (peek.isStartElement) new SimpleAttributeMap(peek.asStartElement.attributes map { o ⇒ o.getName.getLocalPart -> o.getValue })
      else null
  }

  def attributeMap = {
    requireState(_simpleAttributeMap ne null, s"No attributes possible here, at $locationString")
    _simpleAttributeMap
  }

  def locationString = locationToString(peek.getLocation)

  @tailrec
  def peek: XMLEvent =
    delegate.peek match {
      case e if xmlEventIsIgnored(e) =>
        delegate.nextEvent()
        peek
      case e ⇒ e
    }

  def xmlEventReader: XMLEventReader = delegate
}


object ScalaXMLEventReader {

  def parseString[A](xml: String, inputFactory: XMLInputFactory = getCommonXMLInputFactory())(parse: ScalaXMLEventReader ⇒ A): A =
    parseDocument(StringSource(xml), inputFactory)(parse)

  def parseDocument[A](source: Source, inputFactory: XMLInputFactory = getCommonXMLInputFactory())(parse: ScalaXMLEventReader ⇒ A): A =
    autoClosing(new ScalaXMLEventReader(newXMLEventReader(inputFactory, source))) { reader ⇒
      reader.eat[StartDocument]
      val result = parse(reader)
      reader.eat[EndDocument]
      result
    }

  def parse[A](source: Source, inputFactory: XMLInputFactory = getCommonXMLInputFactory())(parseEvents: ScalaXMLEventReader ⇒ A): A = {
    autoClosing(new ScalaXMLEventReader(newXMLEventReader(inputFactory, source))) { reader ⇒
      parseEvents(reader)
    }
  }

  private def newXMLEventReader(inputFactory: XMLInputFactory, source: Source) =
    inputFactory.createXMLEventReader(source)
    //inputFactory.createFilteredReader(inputFactory.createXMLEventReader(source), IgnoreWhitespaceFilter)

  private def xmlEventIsIgnored(e: XMLEvent) =
    cond(e) {
      case e: Characters if e.isWhiteSpace ⇒ true
      case _: Comment ⇒ true
    }

  object IgnoreWhitespaceFilter extends EventFilter {
    def accept(e: XMLEvent) = cond(e) { case e: Characters ⇒ !e.isWhiteSpace }
  }

  final class SimpleAttributeMap private[xmls](pairs: TraversableOnce[(String, String)]) extends mutable.HashMap[String, String] {
    this ++= pairs
    private val readAttributes = mutable.HashSet[String]()
    readAttributes.sizeHint(size)

    def asConverted[A](name: String)(convert: String ⇒ A): A =
      getAsConverted(name)(convert) getOrElse { throw new NoSuchElementException(s"XML attribute '$name' expected") }

    def getAsConverted[A](name: String)(convert: String ⇒ A): Option[A] =
      get(name) map {
        try convert
        catch { case NonFatal(t) ⇒ throw new IllegalArgumentException(s"XML attribute '$name': $t", t) }
      }

    override def apply(o: String): String = {
      readAttributes += o
      super.apply(o)
    }

    override def get(o: String): Option[String] = {
      readAttributes += o
      super.get(o)
    }

    override def default(o: String) = throw new NoSuchElementException(s"XML attribute '$o' is required")

    /** Marks all attribute as read, so that requireAllAttributesRead does not fail. */
    def ignoreUnread(): Unit = keys foreach apply

    def requireAllAttributesRead(): Unit = {
      val names = keySet -- readAttributes
      if (names.nonEmpty) throw new UnparsedAttributesException(names.toImmutableSeq)
    }
  }

  final class ConvertedElementMap[A] private[xmls](pairs: immutable.IndexedSeq[(String, A)]) {

    def one[B <: A : ClassTag]: B =
      option[B] getOrElse { throw new NoSuchElementException(s"No element for type ${implicitClass[B].getSimpleName}") }

    def one[B <: A : ClassTag](elementName: String): B =
      option[B](elementName) getOrElse { throw new NoSuchElementException(s"Element <$elementName> is required") }

    def option[B <: A : ClassTag]: Option[B] = {
      val result = byClass[B]
      require(result.isEmpty || result.tail.isEmpty, s"Element for type ${implicitClass[B].getSimpleName} is allowed only once")
      result.headOption map cast[B]
    }

    def option[B <: A : ClassTag](elementName: String): Option[B] = {
      val result = byName[B](elementName)
      require(result.length <= 1, s"Element <$elementName> is allowed only once")
      result.headOption map cast[B]
    }

    def byClass[B <: A : ClassTag]: immutable.IndexedSeq[B] =
      values collect assignableFrom[B]

    def byName[B <: A : ClassTag](elementName: String): immutable.Seq[B] =
      apply(elementName) map cast[B]

    def values: immutable.IndexedSeq[A] =
      pairs map { _._2 }

    def apply(elementName: String): immutable.Seq[A] =
      pairs collect { case (k, v) if k == elementName ⇒ v }
  }

  final class UnparsedAttributesException private[xmls](val names: immutable.Seq[String]) extends RuntimeException {
    override def getMessage = s"Unknown XML attributes " + (names map { "'"+ _ +"'" } mkString ", ")
  }

  final class XmlException(elementName: String, location: Location, override val getCause: Exception) extends RuntimeException {

    override def toString = s"XmlException: $getMessage"
    override def getMessage = s"$nonWrappedCauseString - In $text"

    private def nonWrappedCauseString = nonWrappedCause.toString stripPrefix "java.lang.RuntimeException: "

    @tailrec
    def nonWrappedCause: Throwable = getCause match {
      case cause: XmlException ⇒ cause.nonWrappedCause
      case cause ⇒ cause
    }

    private def text: String =
      s"<$elementName> (${locationToString(location)})" + (
        getCause match {
          case cause: XmlException ⇒ " " + cause.text
          case _ ⇒ ""
        })
  }

  object XmlException {
    def unapply(o: XmlException) = o.getCause match {
      case cause: XmlException ⇒ Some(cause.nonWrappedCause)
      case cause ⇒ Some(cause)
    }
  }

  private def locationToString(o: Location) =
    (Option(o.getSystemId) ++ Option(o.getPublicId)).flatten.mkString(":") + ":" + o.getLineNumber + ":" + o.getColumnNumber
}
