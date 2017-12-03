package com.sos.jobscheduler.base.circeutils

import com.sos.jobscheduler.base.circeutils.CirceUtils._
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
}
