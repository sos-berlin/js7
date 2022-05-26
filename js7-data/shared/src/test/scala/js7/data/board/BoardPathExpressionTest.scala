package js7.data.board

import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.board.BoardPathExpression.{And, ExpectNotice, Or}
import js7.data.board.BoardPathExpressionParser.parse
import org.scalatest.freespec.AnyFreeSpec

final class BoardPathExpressionTest extends AnyFreeSpec
{
  private val A = BoardPath("A")
  private val B = BoardPath("B")
  private val C = BoardPath("C")

  "A" in {
    assert(parse("'A'") == Right(ExpectNotice(BoardPath("A"))))
  }

  "'A' || 'B' && 'C'" in {
    val expr = parse("'A' || 'B' && 'C'").orThrow
    assert(expr ==
      Or(
        ExpectNotice(BoardPath("A")),
        And(
          ExpectNotice(BoardPath("B")),
          ExpectNotice(BoardPath("C")))))

    assert(!expr.eval(Set.empty))
    assert(expr.eval(Set(A)))
    assert(!expr.eval(Set(B)))
    assert(expr.eval(Set(A, B)))
    assert(!expr.eval(Set(C)))
    assert(expr.eval(Set(C, A)))
    assert(expr.eval(Set(C, B)))
    assert(expr.eval(Set(C, A, B)))
  }

  "'A'||'B'&&'C'" in {
    assert(parse("'A'||'B'&&'C'") == Right(
      Or(
        ExpectNotice(BoardPath("A")),
        And(
          ExpectNotice(BoardPath("B")),
          ExpectNotice(BoardPath("C"))))))
  }

  "'FOLDER/B_BOARD.2'" in {
    assert(parse("'FOLDER/B_BOARD.2'") == Right(
      ExpectNotice(BoardPath("FOLDER/B_BOARD.2"))))
  }
}
