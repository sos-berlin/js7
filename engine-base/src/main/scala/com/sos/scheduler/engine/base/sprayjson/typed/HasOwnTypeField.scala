package com.sos.scheduler.engine.base.sprayjson.typed

import spray.json.{RootJsonReader, RootJsonWriter}

/**
  * @author Joacim Zschimmer
  */
trait HasOwnTypeField[A] {

  def classToJsonWriter: Map[Class[_], RootJsonWriter[_]]

  def typeToJsonReader: Map[String, RootJsonReader[_]]

  def typeToClass: Map[String, Class[_ <: A]]
}
