package com.sos.scheduler.engine.base.sprayjson.typed

import spray.json.{RootJsonReader, RootJsonWriter}

/**
  * @author Joacim Zschimmer
  */
trait HasOwnTypeField[A] {

  def classToJsonWriter: Map[Class[_], RootJsonWriter[_]]

  def typeNameToJsonReader: Map[String, RootJsonReader[_]]

  def typeNameToClass: Map[String, Class[_ <: A]]

  final lazy val classToTypeName: Map[Class[_ <: A], String] = typeNameToClass map { _.swap }
  final lazy val typeNames: Set[String] = typeNameToClass.keySet
}
