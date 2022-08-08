package js7.common.guice

import com.google.inject.TypeLiteral
import com.google.inject.util.Types
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag
import java.lang.reflect.Type

object Guices {
  def typeLiteral[T](implicit T: Tag[T]): TypeLiteral[T] =
    TypeLiteral.get(toType(T.tag))
      .asInstanceOf[TypeLiteral[T]]

  private def normalizeJavaSimpleType(c: Class[_]): Class[_] =
    c match {
      case java.lang.Byte.TYPE => classOf[java.lang.Byte]
      case java.lang.Short.TYPE => classOf[java.lang.Short]
      case java.lang.Character.TYPE => classOf[java.lang.Character]
      case java.lang.Integer.TYPE => classOf[java.lang.Integer]
      case java.lang.Long.TYPE => classOf[java.lang.Long]
      case java.lang.Float.TYPE => classOf[java.lang.Float]
      case java.lang.Double.TYPE => classOf[java.lang.Double]
      case java.lang.Boolean.TYPE => classOf[java.lang.Boolean]
      case java.lang.Void.TYPE => classOf[java.lang.Void]
      case c => c
    }

  def toType(tag: LightTypeTag): Type = {
    val name = tag.withoutArgs.longName
    maybeScalaSimpleType(name) getOrElse otherType(tag, name)
  }

  private def maybeScalaSimpleType(name: String): Option[Type] =
    PartialFunction.condOpt(name) {
      case "scala.Byte" => classOf[java.lang.Byte]
      case "scala.Short" => classOf[java.lang.Short]
      case "scala.Char" => classOf[java.lang.Character]
      case "scala.Int" => classOf[java.lang.Integer]
      case "scala.Long" => classOf[java.lang.Long]
      case "scala.Float" => classOf[java.lang.Float]
      case "scala.Double" => classOf[java.lang.Double]
      case "scala.Boolean" => classOf[java.lang.Boolean]
      case "scala.Unit" => classOf[java.lang.Void]
      case "scala.Any" => classOf[java.lang.Object]
    }

  private def otherType(tag: LightTypeTag, name: String) =
    name.lastIndexOf('.') match {
      case dot if dot > 0 =>
        if (name(dot - 1) == '$') { // Scala 3
          // Remove $ from nested class name
          val cls = Class.forName(name.substring(0, dot) + name.substring(dot + 1))
          val ownerClass = Class.forName(name.substring(0, dot - 1))
          val args = tag.typeArgs
          if (args.isEmpty)
            cls
          else Types.newParameterizedTypeWithOwner(
            ownerClass,
            cls,
            tag.typeArgs.map(toType) *)
        } else // Scala 2
          maybeClass(name.substring(0, dot)) match {
            case None => standardClassToType(tag, name)
            case Some(ownerClass) =>
              // Replace last "." with "$"
              val cls = Class.forName(name.substring(0, dot) + "$" + name.substring(dot + 1))
              nestedClassToType(ownerClass, tag, cls)
          }

      case _ =>
        standardClassToType(tag, name)
    }

  private def maybeClass(name: String) =
    try Some(Class.forName(name))
    catch { case _: ClassNotFoundException =>
      None
    }

  private def nestedClassToType[A, B](ownerClass: Class[A], tag: LightTypeTag, cls: Class[B]) = {
    val args = tag.typeArgs
    if (args.isEmpty)
      cls
    else Types.newParameterizedTypeWithOwner(
      ownerClass,
      cls,
      tag.typeArgs.map(toType) *)
  }

  private def standardClassToType(tag: LightTypeTag, name: String) = {
    val cls = normalizeJavaSimpleType(Class.forName(name))
    val args = tag.typeArgs
    if (args.isEmpty)
      cls
    else Types.newParameterizedType(
      cls,
      tag.typeArgs.map(toType) *)
  }
}
