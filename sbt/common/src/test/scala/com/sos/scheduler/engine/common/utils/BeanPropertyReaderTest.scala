package com.sos.scheduler.engine.common.utils

import com.sos.scheduler.engine.common.utils.BeanPropertyReader.Keep
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class BeanPropertyReaderTest extends FreeSpec {

  "toMap" in {
    class Bean {
      def getInt = 7
      def getString = "STRING"
      def getOther = fail()
    }
    val m = BeanPropertyReader.toMap(new Bean) {
      case "int" ⇒ Keep
      case "string" ⇒ { case v ⇒ s"*$v*" }
    }
    assert(m == Map("int" → 7, "string" → "*STRING*"))
  }

  "beanToMap" in {
    class Bean {
      def getInt = 7
      def getString = "STRING"
    }
    val m = BeanPropertyReader.beanToMap(new Bean)
    assert(m == Map("class" → classOf[Bean], "int" → 7, "string" → "STRING"))
  }
}
