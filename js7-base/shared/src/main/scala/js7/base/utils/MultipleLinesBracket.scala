package js7.base.utils

import fs2.Stream

/** A bracket spanning multiple lines. */
final case class MultipleLinesBracket(first: Char, middle: Char, last: Char, single: Char = ' ')

object MultipleLinesBracket:

  val Round = MultipleLinesBracket("⎛⎜⎝")
  val Square = MultipleLinesBracket("⎡⎢⎣")
  val SmallSquare = MultipleLinesBracket("┌│└")

  def apply(chars: String): MultipleLinesBracket =
    def f(i: Int) = if chars.length > i then chars(i) else ' '
    MultipleLinesBracket(f(0), f(1), f(2), f(3))

  def normalize(brackets: MultipleLinesBracket | String): MultipleLinesBracket =
    brackets match
      case bracket: MultipleLinesBracket => bracket
      case brackets: String => MultipleLinesBracket(brackets)

  def streamInBrackets[A](name: String, brackets: MultipleLinesBracket | String = SmallSquare)
    (iterableOrStream: Iterable[A] | Stream[fs2.Pure, A])
  : Stream[fs2.Pure, String] =
    Stream(name, "=\n") ++ bracketLineStream(brackets)(iterableOrStream)

  def bracketLineStream[A](brackets: MultipleLinesBracket | String = SmallSquare)
    (iterableOrStream: Iterable[A] | Stream[fs2.Pure, A])
  : Stream[fs2.Pure, String] =
    iterableOrStream.match
      case o: Stream[fs2.Pure, A] => o
      case o: Iterable[A] => Stream.iterable(o)
    // Keep chunk small because a single element may be big
    .chunkLimit(1).unchunks
    .zipWithBracket(brackets).flatMap: (o, br) =>
      Stream(" ", br.toString, o.toString, "\n")

  extension[F[_], O](stream: Stream[F, O])
    def zipWithBracket(bracket: MultipleLinesBracket | String): Stream[F, (O, Char)] =
      val br = MultipleLinesBracket.normalize(bracket)
      stream.through:
        _.zipWithPreviousAndNext.map:
          case (None, o, Some(_)) => o -> br.first
          case (Some(_), o, Some(_)) => o -> br.middle
          case (Some(_), o, None) => o -> br.last
          case (None, o, None) => o -> br.single
