package js7.base.utils

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

