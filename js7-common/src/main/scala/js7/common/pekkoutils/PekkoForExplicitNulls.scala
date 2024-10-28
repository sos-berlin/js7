package js7.common.pekkoutils

import org.apache.pekko.http.javadsl.model as jm
import org.apache.pekko.http.scaladsl.model.headers.`Content-Type`
import org.apache.pekko.http.scaladsl.model.{HttpHeader, HttpMessage}
import scala.collection.immutable
import scala.reflect.{ClassTag, classTag}

object PekkoForExplicitNulls:

  extension (self: HttpMessage)
    // Copy from private Pekko's HttpHeader.fastFind, ©️Lightbend Inc., adapted to -Yexplicit-nulls
    /**  Pekko's HttpHeader#header[T] adapted to Scala 3.5 Yexplicit-nulls. */
    def header3[T <: jm.HttpHeader : ClassTag]: Option[T] =
      import self.{entity, headers}
      val clazz = classTag[T].runtimeClass.asInstanceOf[Class[T]]
      fastFind[T](clazz, headers) match
        case Some(h) => Some(h)
        case _ if clazz == classOf[`Content-Type`] =>
          Some(`Content-Type`(entity.contentType)).asInstanceOf[Option[T]]
        case _ => None

  // Copy from private Pekko's HttpHeader.fastFind, ©️Lightbend Inc., adapted to -Yexplicit-nulls
  private def fastFind[T <: jm.HttpHeader](
    clazz: Class[T],
    headers: immutable.Seq[HttpHeader])
  : Option[T] =
    val it = headers.iterator
    while it.hasNext do
      it.next() match
        case h if clazz.isInstance(h) =>
          return Some[T](h.asInstanceOf[T])
        case _ => // continue ...
    None
