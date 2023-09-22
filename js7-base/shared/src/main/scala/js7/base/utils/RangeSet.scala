package js7.base.utils

import cats.parse.Parser
import cats.syntax.semigroupal.*
import cats.syntax.traverse.*
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import js7.base.circeutils.CirceUtils.*
import js7.base.parser.BasicParsers
import js7.base.problem.Checked
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Ordinal.syntax.*
import js7.base.utils.RangeSet.{range, *}
import js7.base.utils.RangeSetParser.{delimiter, rangeSymbol}
import org.jetbrains.annotations.TestOnly
import scala.collection.{immutable, mutable}
import scala.math.Ordering.Implicits.*

/** A Set which allows ranges. */
final case class RangeSet[A: Ordering: Ordinal] private(ranges: Vector[Range[A]])
extends immutable.Set[A]
{
  //override protected def fromSpecific(coll: IterableOnce[A]): Set[A] =
  //  RangeSet.fromIterable(coll)
  //
  //override protected def newSpecificBuilder =
  //  new RangeSet.Builder[A]

  def contains(a: A) =
    ranges.exists(_.contains(a))

  override def concat(that: IterableOnce[A]): RangeSet[A] =
    that match {
      case that: RangeSet[A] @unchecked => fromRanges(ranges ++ that.ranges)
      case _ => fromRanges(ranges ++ RangeSet.fromIterable(that).ranges)
    }

  override def subsetOf(that: collection.Set[A]): Boolean =
    that match {
      case that: RangeSet[A] => equals(that) || ranges.forall(that.containsRange)
      case _ => forall(that)
    }

  private def containsRange(range: Range[A]): Boolean =
    ranges.exists(range.subsetOf)

  def incl(elem: A): RangeSet[A] =
    fromRanges(ranges :+ Single(elem))

  def excl(elem: A): RangeSet[A] =
    ranges.indexWhere(_ contains elem) match {
      case -1 => this
      case i =>
        fromRanges(
          ranges.view
            .take(i)
            .concat(ranges(i) match {
              case Single(_) => Nil

              case Interval(`elem`, end) =>
                range(elem.succ, end) :: Nil

              case Interval(start, `elem`) =>
                range(start, elem.pred) :: Nil

              case Interval(start, end) =>
                range(start, elem.pred) :: range(elem.succ, end) :: Nil
            })
            .concat(ranges.view.drop(i + 1)))
    }

  def iterator: Iterator[A] =
    ranges.iterator.flatMap(_.iterator)

  override def equals(that: Any): Boolean =
    that match {
      case that: RangeSet[A] @unchecked => ranges == that.ranges
      case _ => super.equals(that)
    }

  override def toString =
    ranges.mkString("RangeSet(", delimiter, ")")

  def asString(implicit encoder: Encoder[A]): String =
    ranges
      .map {
        case Single(a) => valueToString(a)
        case Interval(a, b) => s"${valueToString(a)}$rangeSymbol${valueToString(b)}"
      }
      .mkString(delimiter)
}

object RangeSet {

  def apply[A: Ordering: Ordinal](values: A*): RangeSet[A] =
    fromIterable(values)

  def empty[A: Ordering: Ordinal]: RangeSet[A] =
    new RangeSet(Vector.empty[Range[A]])

  def one[A: Ordering: Ordinal](value: A): RangeSet[A] =
    apply(value :: Nil*)

  def fromIterable[A: Ordering: Ordinal](values: IterableOnce[A]): RangeSet[A] =
    fromRanges(values.iterator.map(Single(_)))

  @TestOnly
  private[utils] def raw[A: Ordering: Ordinal](ranges: Range[A]*): RangeSet[A] =
    new RangeSet(ranges.toVector)

  def fromRanges[A: Ordering: Ordinal](ranges: IterableOnce[Range[A]]): RangeSet[A] =
    new RangeSet(normalize(Vector.from(ranges)))

  def parseInt(string: String): Checked[RangeSet[Int]] =
    parse(BasicParsers.int, string)

  def parse[A: Ordering: Ordinal](parseValue: Parser[A], string: String)
  : Checked[RangeSet[A]] =
    new RangeSetParser[A](parseValue).parse(string)

  private def normalize[A: Ordering: Ordinal](ranges: Vector[Range[A]]): Vector[Range[A]] = {
    val buffer = mutable.Buffer[Range[A]]()
    val it = ranges.sortBy(_.start).iterator
    if (it.hasNext) buffer += it.next()

    while (it.hasNext) {
      val next = it.next()
      val last = buffer.last
      if (last.end >= next.start) {
        // Merge overlapping ranges
        buffer(buffer.length - 1) = range(last.start, last.end max next.end)
      } else if (next.start isSuccessorOf last.end) {
        // Merge successive ranges
        buffer(buffer.length - 1) = range(last.start, next.end)
      } else {
        buffer += next
      }
    }

    buffer
      .view
      // Normalize two-element ranges to two Singles: 1-2 => 1, 2
      .flatMap {
        case Interval(a, b) if b isSuccessorOf a =>
          Single(a) :: Single(b) :: Nil
        case o =>
          o :: Nil
      }
      .toVector
  }

  // Do not mix with scala.collection.immutable.Range
  sealed trait Range[A] {
    def start: A
    def end: A
    def contains(a: A): Boolean
    def subsetOf(other: Range[A]): Boolean
    def iterator: Iterator[A]
  }

  // `range` function because Range collides with scala.collection.immutable.Range
  def range[A: Ordering: Ordinal](start: A, end: A): Range[A] =
    if (start == end)
      Single(start)
    else
      Interval(start, end)

  final case class Single[A](value: A) extends Range[A] {
    def start = value
    def end = value
    def contains(a: A) = a == value

    def subsetOf(other: Range[A]): Boolean =
      value == other.start && value == other.end

    def iterator = Iterator.single(value)
    override def toString = value.toString
  }

  /** A closed (inclusive) interval. */
  final case class Interval[A: Ordering: Ordinal] private[RangeSet](start: A, end: A)
  extends Range[A]
  {
    def contains(a: A) = a >= start && a <= end

    def subsetOf(other: Range[A]): Boolean =
      start >= other.start && end <= other.end

    def iterator = Iterator.iterate(start)(_.succ).takeWhile(_ <= end)
    override def toString = start.toString + rangeSymbol + end
  }
  object Interval {
    def apply[A: Ordering: Ordinal](start: A, end: A): Interval[A] = {
      // start.succ == end is allowed, but `normalize` will separate it into two `Single`s
      assertThat(start < end)
      new Interval(start, end)
    }
  }

  // Must be explicitly and locally imported, !!!
  // otherwise it collides with Circe's Decoder[Iterable[A]].
  implicit def jsonEncoder[A: Ordering: Encoder]: Encoder[RangeSet[A]] =
    rangeSet => Json.fromString(rangeSet.asString)

  private def valueToString[A: Encoder](a: A): String =
    a.asJson
      .asNumber
      .map(_.toString)
      .getOrElse(
        a.asJson.asString.getOrElse(
          throw new IllegalArgumentException(s"Encoding as JSON string expected: $a")))

  implicit val IntJsonDecoder: Decoder[RangeSet[Int]] =
    jsonDecoder(BasicParsers.int)

  implicit def jsonDecoder[A: Ordering : Ordinal: Decoder](implicit parseValue: Parser[A])
  : Decoder[RangeSet[A]] =
    jsonDecoder[A](parseValue)

  def jsonDecoder[A: Ordering : Ordinal: Decoder](parseValue: Parser[A]): Decoder[RangeSet[A]] =
    c =>
      c.value.asArray match {
        case None =>
          for {
            string <- c.as[String]
            rangeSet <- parse(parseValue, string).toDecoderResult(c.history)
          } yield rangeSet

        case Some(array) =>
          array
            .traverse(_.as(asArrayRangeDecoder[A]))
            .map(fromRanges(_))
      }

  @TestOnly
  private[utils] def asArrayJsonEncoder[A: Ordering: Encoder]: Encoder.AsArray[RangeSet[A]] =
    rangeSet =>
      rangeSet.ranges.map {
        case Single(a) => a.asJson
        case Interval(a, b) => Json.arr(a.asJson, b.asJson)
      }

  @TestOnly
  private[utils] def asArrayJsonDecoder[A: Ordering: Ordinal: Decoder]: Decoder[RangeSet[A]] =
    c =>
      c.value.asArray match {
        case None => Left(DecodingFailure("RangeSet must be an array", c.history))
        case Some(array) =>
          array
            .traverse(_.as(asArrayRangeDecoder[A]))
            .map(fromRanges(_))
      }

  @TestOnly
  private def asArrayRangeDecoder[A: Ordering : Ordinal : Decoder]: Decoder[Range[A]] =
    c => {
      val json = c.value
      if (json.isArray)
        json.asArray.get match {
          case Vector(a, b) =>
            a.as[A]
              .product(b.as[A])
              .map { case (a, b) =>
                Interval(a, b)
              }
          case _ =>
            Left(DecodingFailure(s"Invalid range: $json", c.history))
        }
      else
        json.as[A].map(Single(_))
    }

  //<editor-fold desc="// Unused class Builder">

  private final class Builder[A: Ordering: Ordinal]
  extends mutable.Builder[A, RangeSet[A]]
  {
    private val buffer = mutable.Buffer[Range[A]]()

    def clear(): Unit =
      buffer.clear()

    // Optimize for RangeSet?
    //override def addAll(xs: IterableOnce[A]): this.type = ...

    def addOne(a: A): this.type = {
      buffer += Single(a)
      this
    }

    def addRange(start: A, end: A): this.type = {
      if (start <= end) {
        buffer += (
          if (start == end)
            Single(start)
          else
            Interval(start, end))
      }
      this
    }

    def isEmpty: Boolean =
      buffer.isEmpty

    def last: Range[A] =
      buffer.last

    def result(): RangeSet[A] =
      fromRanges(buffer)
  }
  //</editor-fold>
}
