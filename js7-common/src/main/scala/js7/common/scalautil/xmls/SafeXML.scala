package js7.common.scalautil.xmls

import javax.xml.parsers.SAXParserFactory
import js7.common.xml.XxeVulnerability
import scala.xml.factory.XMLLoader

/**
 * Inhibits use of DOCTYPE when parsing XML, to reject XXE attacks.
 * Use instead of scala.xml.XML.load..
 * @author Joacim Zschimmer
 * @see [[XxeVulnerability]]
 */
object SafeXML extends XMLLoader[xml.Elem] {

   override def parser = {
     val f = SAXParserFactory.newInstance()
     f.setNamespaceAware(false)
     XxeVulnerability inhibitFor f
     f.newSAXParser()
   }
 }
