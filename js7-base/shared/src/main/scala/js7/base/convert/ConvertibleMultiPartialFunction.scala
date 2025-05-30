package js7.base.convert

import cats.data.NonEmptyList
import js7.base.convert.ConvertiblePartialFunctions.wrappedConvert

/**
  * Provides methods for convertion of the Iterable result of a PartialFunction (for example a Map).
  *
  * @author Joacim Zschimmer
  */
trait ConvertibleMultiPartialFunction[K, V]:

  this: PartialFunction[K, Iterable[V]] =>

  def as[W](key: K, default: => W)(implicit convert: As[V, W]): W =
    optionAs[W](key) getOrElse default

  def as[W](key: K)(implicit convert: As[V, W]): W =
    val iterator = apply(key).iterator
    if iterator.isEmpty then
      throw new NoSuchElementException(s"Missing ${renderKey(key)}")
    else
      val value = iterator.next()
      val result = wrappedConvert(convert.apply, renderKey(key))(value)
      if iterator.hasNext then throwNotUnique(key)
      result

  def optionAs[W](key: K, default: => Option[W])(using convert: As[V, W]): Option[W] =
    optionAs(key)(using convert) orElse default

  def optionAs[W](key: K)(implicit convert: As[V, W]): Option[W] =
    seqAs(key) match
      case collection.Seq() => None
      case collection.Seq(value) => Some(value)
      case _ => throwNotUnique(key)

  private def throwNotUnique(key: K): Nothing =
    throw new IllegalArgumentException(s"Only one value is allowed for ${renderKey(key)}")

  def nelAs[W](key: K)(implicit convert: As[V, W]): NonEmptyList[W] =
    lift(key) match
      case None => throw new NoSuchElementException(s"Missing ${renderKey(key)}")
      case Some(seq) =>
        if seq.isEmpty then throw new NoSuchElementException(s"${renderKey(key)} must not be empty")
        val c = wrappedConvert(convert.apply, renderKey(key))
        NonEmptyList.fromList(seq.map(c).toList) getOrElse:
          throw new IllegalArgumentException(s"${renderKey(key)} must not be empty")

  def seqAs[W](key: K)(implicit convert: As[V, W]): Seq[W] =
    vectorAs[W](key)

  def vectorAs[W](key: K)(implicit convert: As[V, W]): Vector[W] =
    lift(key) match
      case None => Vector.empty  // Missing is equivalent to empty
      case Some(seq) =>
        val c = wrappedConvert(convert.apply, renderKey(key))
        seq.map(c).toVector

  protected def renderKey(key: K) = s"key '$key'"

// Don't use a Logger here to avoid overwriting a concurrently used logfile before start-up
