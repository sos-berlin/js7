package com.sos.scheduler.engine.data.test

import com.fasterxml.jackson.databind.ObjectMapper
import com.sos.scheduler.engine.data.configuration.EngineJacksonConfiguration
import org.scalatest.FunSuite

/** Zum Test von JSON-Serialisierungen.
  * @author Joacim Zschimmer */
trait JsonTest {
  this: FunSuite =>

  def addJsonTests[A <: AnyRef](obj: A, json: String): Unit = {
    addJsonTests(EngineJacksonConfiguration.newObjectMapper(), obj, json)
  }

  def addJsonTests[A <: AnyRef](objectMapper: ObjectMapper, obj: A, json: String): Unit = {
    val serializerTester = new SerializerTester(objectMapper)

    test("Serialize to JSON") {
      serializerTester.assertObjectIsSerializedTo(obj, json)
    }

    test("Deserialize from JSON") {
      serializerTester.assertJsonIsDeserializedTo(json, obj)
    }
  }
}
