package com.sos.jobscheduler.common.utils

import com.sos.jobscheduler.common.utils.BeanPropertyReader.Keep
import org.scalatest.freespec.AnyFreeSpec

/**
 * @author Joacim Zschimmer
 */
final class BeanPropertyReaderTest extends AnyFreeSpec {

  "toMap" in {
    class Bean {
      def getInt = 7
      def getString = "STRING"
      def getOther = fail()
    }
    val m = BeanPropertyReader.toMap(new Bean) {
      case "int" => Keep
      case "string" => { case v => s"*$v*" }
    }
    assert(m == Map("int" -> 7, "string" -> "*STRING*"))
  }

  "beanToMap" in {
    class Bean {
      def getInt = 7
      def getString = "STRING"
    }
    val m = BeanPropertyReader.beanToMap(new Bean)
    assert(m == Map("class" -> classOf[Bean], "int" -> 7, "string" -> "STRING"))
  }
}
