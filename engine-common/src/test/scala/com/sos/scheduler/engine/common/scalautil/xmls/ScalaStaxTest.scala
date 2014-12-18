package com.sos.scheduler.engine.common.scalautil.xmls

import com.sos.scheduler.engine.common.scalautil.xmls.ScalaStax._
import com.sos.scheduler.engine.common.xml.XmlUtils.loadXml
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */

@RunWith(classOf[JUnitRunner])
final class ScalaStaxTest extends FreeSpec {

  "domElementToStaxSource" in {
    val domElement = loadXml("<a/>").getDocumentElement
    for (i <- 1 to 2)
      getCommonXMLInputFactory().createXMLEventReader(domElementToStaxSource(domElement))
  }
}
