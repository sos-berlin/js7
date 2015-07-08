package com.sos.scheduler.engine.agent.common

import com.sos.scheduler.engine.agent.common.BeanPropertyReader.Keep
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class BeanPropertyReaderTest extends FreeSpec {
  "BeanPropertyReader" in {
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
}
