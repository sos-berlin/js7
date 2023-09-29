package js7.common.scalautil.xmls

import java.util.NoSuchElementException
import javax.xml.stream.events.{Characters, Comment, EndDocument, EndElement, StartDocument, StartElement, XMLEvent}
import javax.xml.stream.{Location, XMLEventReader, XMLInputFactory}
import javax.xml.transform.Source
import js7.base.convert.ConvertiblePartialFunction
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.{cast, implicitClass}
import js7.common.scalautil.AssignableFrom.assignableFrom
import js7.common.scalautil.xmls.ScalaStax.{RichStartElement, getCommonXMLInputFactory}
import js7.common.scalautil.xmls.ScalaXMLEventReader.*
import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions
import scala.reflect.ClassTag

final class ScalaXMLEventReader(delegate: XMLEventReader, config: Config = Config.Default)
extends AutoCloseable {

  private var _simpleAttributeMap: SimpleAttributeMap = null

  def close() = delegate.close()

  def parseDocument[A](body: => A): A = {
    eat[StartDocument]
    val result = body
    eat[EndDocument]
    result
  }

  def ignoreElements(): Unit =
    while peek.isStartElement do {
      ignoreElement()
    }

  def ignoreElement(): Unit = {
    require(delegate.peek.isStartElement)
    delegate.nextEvent()
    while !delegate.peek.isEndElement do {
      if delegate.peek.isCharacters then
        while delegate.peek.isCharacters do delegate.nextEvent()
      else if delegate.peek.isStartElement then
        ignoreElement()
      else if delegate.peek.isInstanceOf[Comment] then
        delegate.nextEvent()
      else
        throw new IllegalArgumentException(s"Unknown XML event: ${delegate.peek}")
    }
    delegate.nextEvent()
  }

  def parseElement[A](name: String, withAttributeMap: Boolean = true)(body: => A): A = {
    requireStartElement(name)
    parseElement(withAttributeMap = withAttributeMap)(body)
  }

  def parseElement[A]()(body: => A): A =
    parseElement(withAttributeMap = true)(body)

  def parseElement[A](withAttributeMap: Boolean)(body: => A): A =
    wrapException {
      if peek.isStartElement then {  // should be
        updateAttributeMap(withAttributeMap = withAttributeMap)
      }
      if withAttributeMap then {  // Otherwise, let the caller eat the StartElement to give it access to attributes
        eat[StartElement]
      }
      parseStartElement() {
        val result = body
        if config.ignoreUnknown then {
          ignoreElements()
        }
        result
      }
    }

  def matchElement[A](pf: PartialFunction[String, A]): A =
    pf.applyOrElse(peek.asStartElement.getName.getLocalPart,
      (_: String) => throw new IllegalArgumentException(s"Unexpected XML element: <${peek.asStartElement.getName}>"))

  def parseStartElement[A]()(body: => A): A = {
    val result = body
    eat[EndElement]
    result
  }

  def parseEachRepeatingElement[A](name: String)(body: => A): IndexedSeq[A] =
    parseElements[A] { case `name` => parseElement() { body } }.map(_._2)

  def forEachStartElement[A](body: PartialFunction[String, A]): ConvertedElementMap[A] =
    new ConvertedElementMap(parseElements[A](body))

  def parseElements[A](body: PartialFunction[String, A]): IndexedSeq[(String, A)] = {
    val results = Vector.newBuilder[(String, A)]
    val liftedBody = body.lift
    @tailrec def g(): Unit = peek match {
      case e: StartElement =>
        for o <- parseStartElementAlternative(liftedBody) do
          results += e.getName.toString -> o
        g()
      case _: EndElement =>
      case _: EndDocument =>
    }
    g()
    results.result()
  }

  private[xmls] def parseStartElementAlternative[A](body: String => Option[A]): Option[A] =
    wrapException {
      val name = peek.asStartElement.getName.toString
      val result = body(name)
      if result.isEmpty then {
        if !config.ignoreUnknown then sys.error(s"Unexpected XML element <$name>")
        ignoreElement()
      }
      result
    }

  private def wrapException[A](body: => A): A = {
    val element = peek.asStartElement()
    try body
    catch {
      case x: Exception => throw new XmlException(element.getName.toString, element.getLocation, x)
    }
  }

//  import javax.xml.transform.{Result, TransformerFactory}
//  import js7.common.scalautil.StringWriters.writingString
//  import javax.xml.transform.stax.StAXSource
//  import javax.xml.transform.stream.StreamResult
//  private lazy val transformerFactory = TransformerFactory.newInstance()
//  private[xml] def parseElementAsXmlString(): String =
//    writingString { writer =>
//      parseElementInto(new StreamResult(writer))
//    }
//
//  private def parseElementInto(result: Result): Unit =
//  Liest bis zum Stream-Ende statt nur bis zum Endetag:  transformerFactory.newTransformer().transform(new StAXSource(xmlEventReader), result)

  def requireStartElement(name: String): StartElement = {
    val e = peek.asStartElement
    require(e.getName.getLocalPart == name, s"Not the expected XML element <$name>: <${e.getName}>")
    e
  }

  def eatText(): String = {
    val result = new StringBuilder
    while peek.isCharacters do
      result append eat[Characters].getData
    result.toString()
  }

  def eat[E <: XMLEvent: ClassTag]: E = {
    val e = implicitClass[E]
    if e != classOf[StartElement] then {
      releaseAttributeMap()
    }
    val event = peek  // Skips ignorables
    delegate.nextEvent()
    if !e.isAssignableFrom(event.getClass) then
      throw new IllegalArgumentException(s"'${e.getSimpleName}' expected but '${event.getClass.getSimpleName}' encountered")
    event.asInstanceOf[E]
  }

  def nextEvent() = delegate.nextEvent()

  def hasNext: Boolean = delegate.hasNext

  private def updateAttributeMap(withAttributeMap: Boolean): Unit = {
    releaseAttributeMap()
    _simpleAttributeMap =
      if withAttributeMap && peek.isStartElement then
        new SimpleAttributeMap(peek.asStartElement.attributes filter { a =>
          !IgnoredAttributeNamespaces(a.getName.getNamespaceURI)
        } map { a =>
          a.getName.getLocalPart -> a.getValue }
        )
      else
        null
  }

  private def releaseAttributeMap(): Unit = {
    if !config.ignoreUnknown && _simpleAttributeMap != null then {
      _simpleAttributeMap.requireAllAttributesRead()
    }
    _simpleAttributeMap = null
  }

  def attributeMap: SimpleAttributeMap = {
    if _simpleAttributeMap eq null then throw new IllegalStateException(s"No attributes possible here, at $locationString")
    _simpleAttributeMap
  }

  def locationString: String =
    locationToString(peek.getLocation)

  @tailrec
  def peek: XMLEvent =
    delegate.peek match {
      case e if xmlEventIsIgnorable(e) =>
        delegate.nextEvent()
        peek
      case e => e
    }

  def xmlEventReader: XMLEventReader = delegate
}

object ScalaXMLEventReader {

  private val IgnoredAttributeNamespaces = Set("http://www.w3.org/2001/XMLSchema-instance")

  final case class Config(ignoreUnknown: Boolean = false)

  object Config {
    val Default = Config()
  }

  implicit def scalaXMLEventReaderToXMLEventReader(o: ScalaXMLEventReader): XMLEventReader = o.xmlEventReader

  def parseDocument[A](source: Source, inputFactory: XMLInputFactory = getCommonXMLInputFactory(), config: Config = Config.Default)
    (parse: ScalaXMLEventReader => A)
  : A =
    autoClosing(new ScalaXMLEventReader(newXMLEventReader(inputFactory, source), config = config)) { reader =>
      reader.parseDocument {
        parse(reader)
      }
    }

  private def newXMLEventReader(inputFactory: XMLInputFactory, source: Source) =
    inputFactory.createXMLEventReader(source)
    //inputFactory.createFilteredReader(inputFactory.createXMLEventReader(source), IgnoreWhitespaceFilter)

  private def xmlEventIsIgnorable(e: XMLEvent) =
    e match {
      case e: Characters => e.isWhiteSpace
      case _: Comment => true
      case _ => false
    }

  final class SimpleAttributeMap private[ScalaXMLEventReader](pairs: Iterator[(String, String)])
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
      if keySet != readAttributes then {
        val names = keySet.toSet -- readAttributes
        if names.nonEmpty then throw new UnparsedAttributesException(names.toSeq)
      }
    }
  }

  final class ConvertedElementMap[A] private[xmls](pairs: IndexedSeq[(String, A)]) {

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
      require(result.lengthIs <= 1, s"Element <$elementName> is allowed only once")
      result.headOption map cast[B]
    }

    def byClass[B <: A : ClassTag]: IndexedSeq[B] =
      values collect assignableFrom[B]

    def byName[B <: A : ClassTag](elementName: String): Seq[B] =
      apply(elementName) map cast[B]

    def values: IndexedSeq[A] =
      pairs.map(_._2)

    def apply(elementName: String): Seq[A] =
      pairs collect { case (k, v) if k == elementName => v }
  }

  final class UnparsedAttributesException private[xmls](val names: Seq[String]) extends RuntimeException {
    override def getMessage = "Unknown XML attributes " + names.map("'" + _ + "'").mkString(", ")
  }

  final class XmlException(elementName: String, location: Location, override val getCause: Exception) extends RuntimeException {

    override def toString = s"XmlException: $getMessage"
    override def getMessage = s"$nonWrappedCauseString - In $text"

    private def nonWrappedCauseString = nonWrappedCause.toString stripPrefix "java.lang.RuntimeException: "

    @tailrec
    def nonWrappedCause: Throwable = getCause match {
      case cause: XmlException => cause.nonWrappedCause
      case cause => cause
    }

    private def text: String =
      s"<$elementName> (${locationToString(location)})" + (
        getCause match {
          case cause: XmlException => " " + cause.text
          case _ => ""
        })
  }

  object XmlException {
    def unapply(o: XmlException): Some[Throwable] =
      o.getCause match {
        case cause: XmlException => Some(cause.nonWrappedCause)
        case cause => Some(cause)
      }
  }

  private def locationToString(o: Location) =
    (Option(o.getSystemId) ++ Option(o.getPublicId)).flatten.mkString(":") + ":" + o.getLineNumber + ":" + o.getColumnNumber
}
