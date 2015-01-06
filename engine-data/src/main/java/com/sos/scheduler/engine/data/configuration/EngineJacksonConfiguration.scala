package com.sos.scheduler.engine.data.configuration

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.datatype.guava.GuavaModule
import com.fasterxml.jackson.module.scala.DefaultScalaModule

object EngineJacksonConfiguration {

  def newObjectMapper() = {
    val objectMapper = new ObjectMapper
    objectMapper.configure(JsonGenerator.Feature.AUTO_CLOSE_TARGET, false)
    modifyObjectMapper(objectMapper)
    objectMapper
  }

  def modifyObjectMapper(o: ObjectMapper): Unit = {
    o.registerModule(DefaultScalaModule)
    o.registerModule(new GuavaModule)
  }
}
