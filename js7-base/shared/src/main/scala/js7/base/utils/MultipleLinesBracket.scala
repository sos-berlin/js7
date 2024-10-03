package js7.base.utils

/** A bracket spanning multiple lines. */
final case class MultipleLinesBracket(first: Char, middle: Char, last: Char, single: Char = ' ')

object MultipleLinesBracket:
  val Round = MultipleLinesBracket('⎛', '⎜', '⎝')
  val Square = MultipleLinesBracket('⎡', '⎢', '⎣')
  val SmallSquare = MultipleLinesBracket('┌', '│', '└')
