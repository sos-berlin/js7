package com.sos.jobscheduler.base.circeutils

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec
import scala.collection.immutable._

/**
  * @author Joacim Zschimmer
  */
final class CirceUtilsTest extends FreeSpec {

  private case class A(a: Int, b: B)
  private case class B(string: String, array: Seq[Int], empty: Seq[Int])

  private implicit val BCodec = deriveCirceCodec[B]
  private implicit val ACodec = deriveCirceCodec[A]

  "PrettyPrinter" in {
    assert(ACodec(A(1, B("STRING", 1 :: 2 :: Nil, Nil))).asJson.toPrettyString ==
      """{
      |  "a": 1,
      |  "b": {
      |    "string": "STRING",
      |    "array": [
      |      1,
      |      2
      |    ],
      |    "empty": []
      |  }
      |}""".stripMargin)
  }

  "listMapCodec" in {
    final case class A(a: Int, listMap: ListMap[Int, String])
    implicit val myListMapCodec = listMapCodec[Int, String]()
    implicit val aCodec = deriveCirceCodec[A]
    testJson(A(0, ListMap(1 → "eins", 2 → "zwei", 3 → "drei", 4 → "vier")),
      """{
        "a": 0,
        "listMap": [
          { "key": 1, "value": "eins" },
          { "key": 2, "value": "zwei" },
          { "key": 3, "value": "drei" },
          { "key": 4, "value": "vier" }
        ]
      }""")
  }
}
