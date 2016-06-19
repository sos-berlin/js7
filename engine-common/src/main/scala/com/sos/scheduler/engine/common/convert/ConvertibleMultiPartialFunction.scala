package com.sos.scheduler.engine.common.convert

import com.sos.scheduler.engine.common.convert.Converters.To
import com.sos.scheduler.engine.common.convert.ConvertiblePartialFunctions.wrappedConvert
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import scala.collection.immutable

/**
  * Provides methods for convertion of the Iterable result of a PartialFunction (for example a Map).
  *
  * @author Joacim Zschimmer
  */
trait ConvertibleMultiPartialFunction[K, V] {
  this: PartialFunction[K, Iterable[V]] ⇒

  def as[W](key: K, default: ⇒ W)(implicit convert: To[V, W]): W =
    optionAs[W](key) getOrElse default

  def as[W](key: K)(implicit convert: To[V, W]): W =
    apply(key) match {
      case Seq() ⇒ throw new NoSuchElementException(s"Missing ${renderKey(key)}")
      case Seq(value) ⇒ wrappedConvert(convert, renderKey(key))(value)
      case _ ⇒ throwNotUnique(key)
    }

  def optionAs[W](key: K, default: ⇒ Option[W])(implicit convert: To[V, W]): Option[W] =
    optionAs(key)(convert) orElse default

  def optionAs[W](key: K)(implicit convert: To[V, W]): Option[W] =
    seqAs(key) match {
      case Seq() ⇒ None
      case Seq(value) ⇒ Some(value)
      case _ ⇒ throwNotUnique(key)
    }

  private def throwNotUnique(key: K) = throw new IllegalArgumentException(s"For ${renderKey(key)}, only one value is possible")

  def seqAs[W](key: K)(implicit convert: To[V, W]): immutable.Seq[W] =
    lift(key) match {
      case None ⇒ Nil  // Missing is equivalent to empty
      case Some(seq) ⇒
        val c = wrappedConvert(convert, renderKey(key))
        seq.toImmutableSeq map c
    }

  protected def renderKey(key: K) = s"key '$key'"
}
