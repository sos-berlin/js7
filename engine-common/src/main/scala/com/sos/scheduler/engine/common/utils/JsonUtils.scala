package com.sos.scheduler.engine.common.utils

import org.jetbrains.annotations.TestOnly

/**
 * @author Joacim Zschimmer
 */
object JsonUtils {
  /**
   * Very simple JSON string quoter.
   *
   * @see http://www.json.org/
   */
  @TestOnly
  def jsonQuote(o: Any): String =
    "\"" +
      (o.toString flatMap {
        case '\\' ⇒ """\\"""
        case '"' ⇒ """\""""
        case c if c >= '\u0000' && c < '\u0020' || c >= '\u0100' && c < '\u0120' ⇒ f"\\u${c.toInt}%04x"
        case c ⇒ c.toString
      }) +
    "\""
}
