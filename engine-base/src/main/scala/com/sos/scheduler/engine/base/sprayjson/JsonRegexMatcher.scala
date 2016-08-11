package com.sos.scheduler.engine.base.sprayjson

import scala.util.control.NonFatal
import scala.util.matching.Regex
import spray.json._

/**
 * @author Joacim Zschimmer
 */
object JsonRegexMatcher {
  val AnyIsoTimestamp = """\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d(.\d+)?Z""".r
  case object AnyInt
  case object AnyLong
  case object AnyString

  /** Wirft eine Exception, falls json nicht dem Muster entspricht. */
  def testRegexJson(json: String, patternMap: Map[String, Any]): Unit = {
    val jsObject = json.parseJson.asJsObject
    require(jsObject.fields.keySet == patternMap.keySet, s"Fields ${jsObject.fields.keySet} does not match expected ${patternMap.keySet}")
    for ((name, expectedValue) ← patternMap) {
      try {
        (jsObject.fields(name), expectedValue) match {
          case (JsString(string), regex: Regex) ⇒
            if (!regex.pattern.matcher(string).matches)
              sys.error(s"JsString '$string' does not match regular expression $regex")
          case (JsNumber(n), AnyInt) ⇒
            if (!n.isValidInt) sys.error(s"Not an Int: $n")
          case (JsNumber(n), AnyLong) ⇒
            if (!n.isValidLong) sys.error(s"Not a Long: $n")
          case (JsString(string), expected: String) if string == expected ⇒
          case (JsString(string), AnyString) ⇒
          case (JsNumber(number), expected: Number) if number == expected ⇒
          case (JsObject(number), AnyRef) ⇒
          case (expected, jsonValue) if jsonValue != expected ⇒
            sys.error(s"Not as expected: json=$jsonValue, expected=$expected")
        }
      } catch {
        case NonFatal(t) ⇒ throw new RuntimeException(s"Error with JSON field $name: $t", t)
      }
    }
  }
}
