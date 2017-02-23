package com.sos.scheduler.engine.jobapi.scripting

import javax.script._
import org.scalatest.Matchers._
import org.scalatest.{FreeSpec, OneInstancePerTest}

final class ScriptEngineTest extends FreeSpec with OneInstancePerTest {

  private val engine = new ScriptEngineManager(null).getEngineByName("JavaScript")

  "testBinding" in {
    val bindings = engine.getBindings(ScriptContext.ENGINE_SCOPE)
    bindings.put("name", "hello")
    assert(engine.get("name") == "hello")
  }

  "testFunction" in {
    engine.eval("function add (a, b) { var c = a + b; return c; }")
    val invocable = engine.asInstanceOf[Invocable]
      Int.box(4)
    invocable.invokeFunction("add", Int.box(10), Int.box(5)).asInstanceOf[Number].doubleValue shouldEqual
      15.0 +- 0.01
  }
}
