package com.sos.jobscheduler.tester

import java.lang.Character.isWhitespace
import spray.json._
import org.scalatest.Assertions._

/**
  * @author Joacim Zschimmer
  */
object SprayJsonTester {

  private[tester] def testSprayJson[A: JsonFormat](a: A, jsonString: String): Unit = {
    assert(a.toJson == jsonString.parseJson)
    assert(jsonString.parseJson.convertTo[A] == a)
    assert(a.toJson.compactPrint.replace("E+", "E")/*remove Spray's exponent sign*/.length == jsonString.count(o ⇒ !isWhitespace(o)))
    assert(a.toJson.compactPrint.parseJson == a.toJson)
  }
}
