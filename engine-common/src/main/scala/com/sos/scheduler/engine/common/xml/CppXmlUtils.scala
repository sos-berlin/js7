package com.sos.scheduler.engine.common.xml

import akka.util.ByteString
import com.google.common.base.MoreObjects.firstNonNull
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.scalautil.ScalaThreadLocal._
import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.scheduler.engine.common.scalautil.StringWriters.writingString
import com.sos.scheduler.engine.common.scalautil.xmls.{SafeXML, ScalaStax}
import com.sos.scheduler.engine.common.xml.XmlUtils._
import com.sos.scheduler.engine.cplusplus.runtime.annotation.ForCpp
import java.io._
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import javax.annotation.Nullable
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.OutputKeys._
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.{StreamResult, StreamSource}
import javax.xml.transform.{Result, Source, TransformerFactory}
import javax.xml.xpath.{XPathConstants, XPathFactory}
import org.w3c.dom.{Document, Element, Node, NodeList}
import org.xml.sax.{ErrorHandler, InputSource, SAXParseException}
import scala.util.matching.Regex

@ForCpp
object CppXmlUtils {

  // See https://www.w3.org/TR/REC-xml/#charsets
  private lazy val InvalidXmlCharactersRegex = new Regex("[" +
    "\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007" +
    "\u0008\u000b\u000c\u000e\u000f" +
    "\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017" +
    "\u0018\u0019\u001a\u001b\u001c\u001d\u001e\u001f" +
    "]")
  private val ReplacementCharacter = '�' ensuring { _ == '\ufffd' }
  private lazy val xPathFactory = newXPathFactory()
  private lazy val xPath = threadLocal { xPathFactory.newXPath() }

  private val logger = Logger(getClass)
  private val documentBuilder = threadLocal {
    val factory = DocumentBuilderFactory.newInstance() sideEffect { o ⇒
      o.setNamespaceAware(true)
      XxeVulnerability inhibitFor o
    }
    factory.newDocumentBuilder() sideEffect {
      _.setErrorHandler(new ErrorHandler {
        def warning(exception: SAXParseException) = logger.debug(exception.toString, exception)
        def error(exception: SAXParseException) = throw exception
        def fatalError(exception: SAXParseException) = throw exception
      })
    }
  }
  private val transformerFactory = threadLocal { TransformerFactory.newInstance() }

  private var static_xPathNullPointerLogged = false

  def domElementToStaxSource(element: org.w3c.dom.Element): Source =
    new StreamSource(new ByteArrayInputStream(toXmlBytes(element)))

  @ForCpp
  def newDocument(): Document = {
    val result = documentBuilder.newDocument()
    postInitializeDocument(result)
    result
  }

  /**
    * Replaces characters between code points 0 and 31 which are illegal in XML, with '�'.
    *
    * @param string
    * @return
    */
  @ForCpp
  def sanitize(string: String): String = InvalidXmlCharactersRegex.replaceAllIn(string, ReplacementCharacter.toString)

  def prettyXml(document: Document): String =
    writingString { w => writeXmlTo(document, new StreamResult(w), encoding = None, indent = true) }

  /** @param encoding "": Codierung steht im XML-Prolog. */
  @ForCpp
  def loadXml(xml: Array[Byte], encoding: String): Document =
    loadXml(new ByteArrayInputStream(xml), encoding)

  private def loadXml(in: InputStream, encoding: String): Document =
    encoding match {
      case "" => loadXml(in)
      case _ => loadXml(new InputStreamReader(in, Charset forName encoding))
    }

  def loadXml(xml: String): Document =
    loadXml(new StringReader(xml))

  def loadXml(in: Reader): Document =
    documentBuilder.parse(new InputSource(in)) sideEffect postInitializeDocument

  def loadXml(in: InputStream): Document =
    documentBuilder.parse(in) sideEffect postInitializeDocument

  private def postInitializeDocument(doc: Document): Unit = {
    doc.setXmlStandalone(true)
  }

  @ForCpp
  def rawXmlToString(xmlBytes: Array[Byte]): String =
    new String(xmlBytes, encoding(xmlBytes))

  @ForCpp
  def toXmlBytes(n: Node, encoding: String, indent: Boolean): Array[Byte] =
    toXmlBytes(n, Charset.forName(encoding), indent = indent)

  def toXmlBytes(n: Node, encoding: Charset = UTF_8, indent: Boolean = false): Array[Byte] = {
    val o = new ByteArrayOutputStream
      writeXmlTo(n, o, encoding, indent = indent)
    o.toByteArray
  }

  def toXml(n: Node): String =
    toXml(n, indent = false)

  @ForCpp
  def toXml(n: Node, indent: Boolean): String = {
    val result = writingString { w => writeXmlTo(n, w, indent = indent) }
    removeXmlProlog(result)
  }

  def writeXmlTo(n: Node, o: OutputStream, encoding: Charset, indent: Boolean): Unit = {
    writeXmlTo(n, new StreamResult(o), Some(encoding), indent = indent)
  }

  def writeXmlTo(n: Node, w: Writer, indent: Boolean = false): Unit = {
    writeXmlTo(n, new StreamResult(w), encoding = None, indent = indent)
  }

  private def writeXmlTo(node: Node, result: Result, encoding: Option[Charset], indent: Boolean): Unit = {
    val transformer = transformerFactory.newTransformer()
    for (o <- encoding) transformer.setOutputProperty(ENCODING, o.name)
    transformer.setOutputProperty(OMIT_XML_DECLARATION, if (encoding.isDefined) "no" else "yes")
    if (indent) {
      transformer.setOutputProperty(INDENT, "yes")
      transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4")
    }
    transformer.transform(new DOMSource(node), result)
  }

  @ForCpp
  def booleanXmlAttribute(xmlElement: Element, attributeName: String, default: Boolean): Boolean = {
    val value = xmlElement.getAttribute(attributeName)
    booleanOptionOf(value, default) getOrElse sys.error(s"Invalid Boolean value in <${xmlElement.getNodeName} $attributeName=${xmlQuoted(value)}>")
  }

  private def booleanOptionOf(s: String, default: Boolean): Option[Boolean] =
    s match {
      case "true" | "yes" | "1" ⇒ Some(true)
      case "false" | "no" | "0" ⇒ Some(false)
      case "" ⇒ Some(default)
      case _ => None
    }

  def xmlStringToBoolean(o: String) = o match {
    case "true" | "yes" | "1" ⇒ true
    case "false" | "no" | "0" ⇒ false
    case _ ⇒ throw new IllegalArgumentException(s"Boolean value expected instead of '$o'")
  }

  @ForCpp
  def intXmlAttribute(xmlElement: Element, attributeName: String): Int =
    intXmlAttribute(xmlElement, attributeName, null.asInstanceOf[Integer])

  @ForCpp
  def intXmlAttribute(xmlElement: Element, attributeName: String, @Nullable defaultValue: Integer): Int = {
    val value = xmlAttribute(xmlElement, attributeName, "")
    if (!value.isEmpty) {
      try Integer.parseInt(value)
      catch {
        case x: NumberFormatException =>
          throw new RuntimeException(s"Invalid numeric value in <${xmlElement.getNodeName} $attributeName=${xmlQuoted(value)}>", x)
      }
    }
    else {
      if (defaultValue == null) throw missingAttributeException(xmlElement, attributeName)
      defaultValue
    }
  }

  @ForCpp
  def xmlAttribute(xmlElement: Element, attributeName: String, @Nullable defaultValue: String): String = {
    val result = xmlElement.getAttribute(attributeName)
    if (!result.isEmpty) result
    else {
      if (defaultValue == null) throw missingAttributeException(xmlElement, attributeName)
      defaultValue
    }
  }

  private def missingAttributeException(e: Element, attributeName: String) =
    new RuntimeException(s"Missing attribute <${e.getNodeName} $attributeName=...>")

  def elementXPath(baseNode: Node, xpathExpression: String): Element =
    elementXPathOption(baseNode, xpathExpression) getOrElse sys.error(s"XPath does not return an element: $xpathExpression")

  @Nullable
  def elementXPathOrNull(baseNode: Node, xpathExpression: String): Element =
    elementXPathOption(baseNode, xpathExpression).orNull

  def elementXPathOption(baseNode: Node, xpathExpression: String): Option[Element] =
    Option(xPath.evaluate(xpathExpression, baseNode, XPathConstants.NODE).asInstanceOf[Element])

  def elementsXPath(baseNode: Node, xpathExpression: String) =
    elementListFromNodeList(xPath.evaluate(xpathExpression, baseNode, XPathConstants.NODESET).asInstanceOf[NodeList])

  def elementListFromNodeList(list: NodeList): IndexedSeq[Element] =
    0 until list.getLength map list.item map { _.asInstanceOf[Element] }

  def stringXPath(baseNode: Node, xpathExpression: String): String = {
    val result = xPath.evaluate(xpathExpression, baseNode, XPathConstants.STRING).asInstanceOf[String]
    if (result == null) sys.error(s"XPath does not match: $xpathExpression")
    result
  }

  def stringXPath(baseNode: Node, xpathExpression: String, default: String): String = {
    val result = xPath.evaluate(xpathExpression, baseNode, XPathConstants.STRING).asInstanceOf[String]
    firstNonNull(result, default)
  }

  def booleanXPath(baseNode: Node, xpathExpression: String): Boolean =
    xPath.evaluate(xpathExpression, baseNode, XPathConstants.BOOLEAN).asInstanceOf[Boolean]

  @ForCpp
  def xpathNodeList(baseNode: Node, xpathExpression: String): NodeList = {
    xPath.evaluate(xpathExpression, baseNode, XPathConstants.NODESET).asInstanceOf[NodeList]
  }

  @ForCpp
  def xpathNode(baseNode: Node, xpathExpression: String): Node =
    xPath.evaluate(xpathExpression, baseNode, XPathConstants.NODE).asInstanceOf[Node]

  private def newXPathFactory(): XPathFactory =
    try XPathFactory.newInstance()
    catch {
      case e: NullPointerException => // JSSIXFOUR-8: Passiert als Dienst unter Windows 2008 mit JDK 1.7.0_09 (64bit?).
        workaroundNewXPathFactory(e)
    }

  private def workaroundNewXPathFactory(e: NullPointerException): XPathFactory = {
    val workAroundClassName = "com.sun.org.apache.xpath.internal.jaxp.XPathFactoryImpl"
    if (!static_xPathNullPointerLogged) {
      logger.debug(s"Trying to use $workAroundClassName as a workaround after $e", e)
      static_xPathNullPointerLogged = true
    }
    try {
      val result = Class.forName(workAroundClassName).asInstanceOf[Class[XPathFactory]].newInstance()
      logger.warn(s"Using $workAroundClassName as a workaround after $e", e)
      result
    }
    catch {
      case ee: Throwable =>
        logger.error("Workaround failed", ee)
        e.addSuppressed(ee)
        throw e
      }
    }

  def xmlQuoted(value: String): String = {
    val result = new StringBuilder(value.length + 20)
    value foreach {
      case '"' => result append "&quot;"
      case '&' => result append "&amp;"
      case '<' => result append "&lt;"
      case o => result append o
      }
    "\""+ result +"\""
  }

  def xmlByteStringToString(byteString: ByteString): String =
    xmlBytesToString(byteString.toArray)

  def xmlBytesToString(bytes: Array[Byte]): String =
    SafeXML.load(new ByteArrayInputStream(bytes)).toString()

  def encoding(xmlBytes: Array[Byte]): Charset = {
    val eventReader = ScalaStax.getCommonXMLInputFactory().createXMLStreamReader(new StreamSource(new ByteArrayInputStream(xmlBytes)))
    Option(eventReader.getEncoding) map Charset.forName getOrElse UTF_8
  }
}

