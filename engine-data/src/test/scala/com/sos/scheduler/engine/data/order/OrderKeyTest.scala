package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.filebased.TypedPath
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.order.OrderKeyTest._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class OrderKeyTest extends FreeSpec {

  "name" in {
    assert((JobChainPath("/a/b") orderKey "1").name == "b,1")
    assert((JobChainPath("/a/b") orderKey "1/2").name == "b,1/2")   // Possible for non-permanent orders
  }

  "asTyped" in {
    assert(APath("/TEST,1").asTyped[OrderKey] == OrderKey(JobChainPath("/TEST"), OrderId("1")))
  }
}

private object OrderKeyTest {
  final case class APath(string: String) extends TypedPath {
    validate()
    def companion = APath
  }
  object APath extends TypedPath.Companion[APath]
}
