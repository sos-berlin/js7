package js7.data.board

import js7.base.test.OurTestSuite
import js7.data.board.BoardPathExpression.{And, ExpectNotice, Or}
import js7.data.board.BoardPathExpressionParser.parse
import org.scalactic.source

final class BoardPathExpressionTest extends OurTestSuite
{
  private val A = BoardPath("A")
  private val B = BoardPath("B")
  private val C = BoardPath("C")

  "A" in {
    testParse("'A'", ExpectNotice(BoardPath("A")))
  }

  "'A' || 'B' && 'C'" in {
    val expr = testParse(
      "'A' || 'B' && 'C'",
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
    testParse("'A'||'B'&&'C'",
      Or(
        ExpectNotice(BoardPath("A")),
        And(
          ExpectNotice(BoardPath("B")),
          ExpectNotice(BoardPath("C")))))
  }

  "'A' && ('B' || 'C')" in {
    val expr = testParse(
      "'A' && ('B' || 'C')",
      And(
        ExpectNotice(BoardPath("A")),
        Or(
          ExpectNotice(BoardPath("B")),
          ExpectNotice(BoardPath("C")))))

    assert(!expr.eval(Set.empty))
    assert(!expr.eval(Set(A)))
    assert(!expr.eval(Set(B)))
    assert(expr.eval(Set(A, B)))
    assert(expr.eval(Set(A, C)))
    assert(expr.eval(Set(A, B, C)))
    assert(!expr.eval(Set(B, C)))
    assert(!expr.eval(Set(C)))
  }

  "'FOLDER/B_BOARD.2'" in {
    assert(parse("'FOLDER/B_BOARD.2'") == Right(
      ExpectNotice(BoardPath("FOLDER/B_BOARD.2"))))
  }

  private def testParse(exprString: String, expr: BoardPathExpression)
    (implicit pos: source.Position)
  : BoardPathExpression = {
    assert(parse(exprString) == Right(expr))
    assert(parse(expr.toString) == Right(expr), " - toString")
    parse(exprString).toOption.get
  }
}
