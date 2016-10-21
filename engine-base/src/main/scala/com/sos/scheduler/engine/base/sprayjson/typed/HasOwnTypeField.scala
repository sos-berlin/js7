package com.sos.scheduler.engine.base.sprayjson.typed

import scala.collection.immutable
import spray.json.{RootJsonReader, RootJsonWriter}

/**
  * @author Joacim Zschimmer
  */
trait HasOwnTypeField[A] {

  def classToJsonWriter: Map[Class[_], RootJsonWriter[_]]

  def typeNameToJsonReader: Map[String, RootJsonReader[_]]

  def typeNameToClass: Map[String, Class[_ <: A]]

  /**
    * Typenames in original ordering.
    */
  def subtypeNames: immutable.Seq[String]

  final lazy val classToTypeName: Map[Class[_ <: A], String] = typeNameToClass map { _.swap }

  final def typeNameExists(typeName: String) = typeNameToClass exists { _._1 == typeName }
}
