package com.sos.scheduler.engine.http.client.idempotence

import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RequestIdTest extends FreeSpec {

  "succ" in {
    assert(RequestId(1).succ == RequestId(2))
  }

  "pred" in {
    assert(RequestId(2).pred == RequestId(1))
  }

  "compare" in {
    assert((RequestId(1) compare RequestId(2)) < 0)
    assert((RequestId(1) compare RequestId(1)) == 0)
    assert((RequestId(2) compare RequestId(1)) > 0)
    assert(RequestId(1) < RequestId(2))
  }

  "Generator" in {
    val newRequestId = new RequestId.Generator
    assert(newRequestId() == RequestId(1))
    assert(newRequestId() == RequestId(2))
  }

  "Eater" in {
    val eat = new RequestId.Eater
    assert(eat(RequestId(11)))
    assert(eat(RequestId(12)))
    assert(!eat(RequestId(12)))
    assert(eat(RequestId(13)))
    assert(!eat(RequestId(15)))
    assert(eat.expectedId == RequestId(14))
  }
}
