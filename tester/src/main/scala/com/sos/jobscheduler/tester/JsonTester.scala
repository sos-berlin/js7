package com.sos.jobscheduler.tester

import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.encoding.DerivedObjectEncoder
import shapeless.Lazy
import spray.json.JsonFormat

/**
  * @author Joacim Zschimmer
  */
object JsonTester {

  def testCirceAndSprayJson[A: JsonFormat](a: A, jsonString: String)(implicit lazyEncoder: Lazy[DerivedObjectEncoder[A]], lazyDecoder: Lazy[DerivedDecoder[A]]): Unit = {
    testCirceJson(a, jsonString)
    testSprayJson(a, jsonString)
  }

  def testCirceJson[A](a: A, jsonString: String)(implicit lazyEncoder: Lazy[DerivedObjectEncoder[A]], lazyDecoder: Lazy[DerivedDecoder[A]]): Unit =
    CirceJsonTester.testCirceJson(a, jsonString)

  def testSprayJson[A: JsonFormat](a: A, jsonString: String): Unit =
    SprayJsonTester.testSprayJson(a, jsonString)
}
