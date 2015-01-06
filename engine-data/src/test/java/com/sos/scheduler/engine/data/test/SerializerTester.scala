package com.sos.scheduler.engine.data.test

import com.fasterxml.jackson.databind.ObjectMapper

/** @author Joacim Zschimmer */
final class SerializerTester(mapper: ObjectMapper) {

  def assertObjectIsSerializedTo(o: AnyRef, expectedJson: String): Unit = {
    val json = mapper.writeValueAsString(o)
    if (mapper.readTree(json) != mapper.readTree(expectedJson))
      throw new AssertionError("Object is not serialized as expected. JSON="+ json +"\n"+"Expected JSON="+ expectedJson)
  }

  def assertJsonIsDeserializedTo(json: String, expectedObject: AnyRef): Unit = {
    val c = expectedObject.getClass
    val o = mapper.readValue(json, c)
    if (o.getClass.getName != c.getName)
      throw new AssertionError("JSON is not deserialized to expected class "+ expectedObject.getClass +", JSON="+ json)
    if (o != expectedObject)
      throw new AssertionError("JSON is not deserialized to expected object "+expectedObject + ", JSON="+json)
  }
}
