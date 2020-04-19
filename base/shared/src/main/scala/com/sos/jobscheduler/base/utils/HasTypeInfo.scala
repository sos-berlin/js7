package com.sos.jobscheduler.base.utils

import scala.reflect.runtime.universe._

trait HasTypeInfo[A]
{
  def typeName: String
}

object HasTypeInfo
{
  private val RemovePackageRegex = """^([a-z0-9]*\.)*""".r

  def apply[A](name: String): HasTypeInfo[A] =
    TypeInfo(name)

  implicit def typeTagToTypeName[A](implicit A: TypeTag[A]): HasTypeInfo[A] = {
    val fullName = A.tpe.toString
    TypeInfo(RemovePackageRegex.replaceFirstIn(fullName, ""))
  }

  private final case class TypeInfo[A](typeName: String)
  extends HasTypeInfo[A]
}
