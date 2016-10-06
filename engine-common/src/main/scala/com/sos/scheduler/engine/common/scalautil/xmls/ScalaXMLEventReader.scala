package com.sos.scheduler.engine.common.scalautil.xmls

import com.sos.scheduler.engine.base.convert.{ConvertiblePartialFunction, ConvertiblePartialFunctions}
import com.sos.scheduler.engine.base.utils.ScalaUtils.{cast, implicitClass}
import com.sos.scheduler.engine.common.scalautil.AssignableFrom.assignableFrom
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaStax.{RichStartElement, getCommonXMLInputFactory, xmlElemToStaxSource}
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader._
import java.util.NoSuchElementException
import javax.xml.stream.events.{Characters, Comment, EndDocument, EndElement, StartDocument, StartElement, XMLEvent}
import javax.xml.stream.{Location, XMLEventReader, XMLInputFactory}
import javax.xml.transform.Source
import org.scalactic.Requirements._
import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.language.implicitConversions
import scala.reflect.ClassTag

final class ScalaXMLEventReader(delegate: XMLEventReader, ignoreUnknown: Boolean = false) extends AutoCloseable {

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

  def ignoreElements(): Unit =
    while(peek.isStartElement) {
      ignoreElement()
    }

  def ignoreElement(): Unit = {
    require(delegate.peek.isStartElement)
    delegate.nextEvent()
    while (!delegate.peek.isEndElement) {
      if (delegate.peek.isCharacters)
        while (delegate.peek.isCharacters) delegate.nextEvent()
      else if (delegate.peek.isStartElement)
        ignoreElement()
      else if (delegate.peek.isInstanceOf[Comment])
        delegate.nextEvent()
      else
        throw new IllegalArgumentException(s"Unknown XML event: ${delegate.peek}")
    }
    delegate.nextEvent()
  }

  def parseElement[A](name: String)(body: ⇒ A): A = {
    requireStartElement(name)
    parseElement()(body)
  }

  def parseElement[A]()(body: ⇒ A): A =
    wrapException {
      eat[StartElement]
      parseStartElement() {
        val result = body
        if (ignoreUnknown) {
          ignoreElements()
        }
        result
      }
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
    val results = Vector.newBuilder[(String, A)]
    @tailrec def g(): Unit = peek match {
      case e: StartElement ⇒
        for (o ← parseStartElementAlternative(f))
          results += e.getName.toString → o
        g()
      case e: EndElement ⇒
      case e: EndDocument ⇒
    }
    g()
    new ConvertedElementMap(results.result)
  }

  def parseStartElementAlternative[A](f: PartialFunction[String, A]): Option[A] =
    wrapException {
      val name = peek.asStartElement.getName.toString
      val result = f.lift(name)
      if (result.isEmpty) {
        if (!ignoreUnknown) sys.error(s"Unexpected XML element <$name>")
        ignoreElement()
      }
      result
    }

  private def wrapException[A](f: ⇒ A): A = {
    val element = peek.asStartElement()
    try f
    catch {
      case x: Exception ⇒ throw new XmlException(element.getName.toString, element.getLocation, x)
    }
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

  def eat[E <: XMLEvent: ClassTag]: E = {
    val event = next()
    val e = implicitClass[E]
    if (!e.isAssignableFrom(event.getClass))
      throw new IllegalArgumentException(s"'${e.getSimpleName}' expected but '${event.getClass.getSimpleName}' encountered")
    event.asInstanceOf[E]
  }

  def hasNext = delegate.hasNext

  def next(): XMLEvent = {
    if (!atStart) {
      updateAttributeMap()
    }
    atStart = false
    delegate.nextEvent()
  }

  private def updateAttributeMap(): Unit = {
    if (!ignoreUnknown && _simpleAttributeMap != null) {
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
      case e if xmlEventIsIgnorable(e) =>
        delegate.nextEvent()
        peek
      case e ⇒ e
    }

  def xmlEventReader: XMLEventReader = delegate
}


object ScalaXMLEventReader {

  implicit def scalaXMLEventReaderToXMLEventReader(o: ScalaXMLEventReader): XMLEventReader = o.xmlEventReader

  def parseString[A](xml: String, inputFactory: XMLInputFactory = getCommonXMLInputFactory())(parse: ScalaXMLEventReader ⇒ A): A =
    parseDocument(StringSource(xml), inputFactory)(parse)

  def parseElem[A](elem: xml.Elem, inputFactory: XMLInputFactory = getCommonXMLInputFactory())(parseEvents: ScalaXMLEventReader ⇒ A): A =
    parseDocument(xmlElemToStaxSource(elem), inputFactory)(parseEvents)

  def parseDocument[A](source: Source, inputFactory: XMLInputFactory = getCommonXMLInputFactory(), ignoreUnknown: Boolean = false)
    (parse: ScalaXMLEventReader ⇒ A)
  : A =
    autoClosing(new ScalaXMLEventReader(newXMLEventReader(inputFactory, source), ignoreUnknown = ignoreUnknown)) { reader ⇒
      reader.parseDocument {
        parse(reader)
      }
    }

  private def newXMLEventReader(inputFactory: XMLInputFactory, source: Source) =
    inputFactory.createXMLEventReader(source)
    //inputFactory.createFilteredReader(inputFactory.createXMLEventReader(source), IgnoreWhitespaceFilter)

  private def xmlEventIsIgnorable(e: XMLEvent) =
    e match {
      case e if e.isCharacters && e.asInstanceOf[Characters].isWhiteSpace ⇒ true
      case _: Comment ⇒ true
      case _ ⇒ false
    }

//  object IgnoreWhitespaceFilter extends EventFilter {
//    def accept(e: XMLEvent) = cond(e) { case e: Characters ⇒ !e.isWhiteSpace }
//  }

  final class SimpleAttributeMap private[xmls](pairs: TraversableOnce[(String, String)])
  extends mutable.HashMap[String, String]
  with ConvertiblePartialFunction[String, String]
  {
    this ++= pairs
    private val readAttributes = mutable.HashSet[String]()
    readAttributes.sizeHint(size)

    override def apply(o: String): String = {
      readAttributes += o
      super.apply(o)
    }

    override def get(name: String): Option[String] = {
      readAttributes += name
      super.get(name)
    }

    override def default(name: String) = throw new NoSuchElementException(s"XML attribute '$name' is required")

    def ignore(name: String): Unit = readAttributes += name

    /** Marks all attributes as read, so that requireAllAttributesRead does not fail. */
    def ignoreUnread(): Unit = readAttributes ++= keys

    def requireAllAttributesRead(): Unit = {
      val names = keySet -- readAttributes
      if (names.nonEmpty) throw new UnparsedAttributesException(names.toImmutableSeq)
    }
  }

  final class ConvertedElementMap[A] private[xmls](pairs: Vector[(String, A)]) {

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
