package js7.data.board

import js7.base.test.OurTestSuite
import js7.base.utils.L3
import js7.base.utils.L3.{False, True, Unknown}
import js7.data.board.BoardPathExpression.{And, ExpectNotice, Or}
import js7.data.board.BoardPathExpressionParser.parse
import org.scalactic.source

final class BoardPathExpressionTest extends OurTestSuite:
  private val A = BoardPath("A")
  private val B = BoardPath("B")
  private val C = BoardPath("C")

  "A" in:
    testParse("'A'", ExpectNotice(BoardPath("A")))

  "'A' || 'B' && 'C'" in:
    val expr = testParse(
      "'A' || 'B' && 'C'",
      Or(
        ExpectNotice(BoardPath("A")),
        And(
          ExpectNotice(BoardPath("B")),
          ExpectNotice(BoardPath("C")))))

    assert(expr.eval(Map(A -> False  , B -> False  , C -> False  )) == False)
    assert(expr.eval(Map(A -> False  , B -> False  , C -> Unknown)) == False)
    assert(expr.eval(Map(A -> False  , B -> False  , C -> True   )) == False)
    assert(expr.eval(Map(A -> False  , B -> Unknown, C -> False  )) == False)
    assert(expr.eval(Map(A -> False  , B -> Unknown, C -> Unknown)) == Unknown)
    assert(expr.eval(Map(A -> False  , B -> Unknown, C -> True   )) == Unknown)
    assert(expr.eval(Map(A -> False  , B -> True   , C -> False  )) == False)
    assert(expr.eval(Map(A -> False  , B -> True   , C -> Unknown)) == Unknown)
    assert(expr.eval(Map(A -> False  , B -> True   , C -> True   )) == True)

    assert(expr.eval(Map(A -> Unknown, B -> False  , C -> False  )) == Unknown)
    assert(expr.eval(Map(A -> Unknown, B -> False  , C -> Unknown)) == Unknown)
    assert(expr.eval(Map(A -> Unknown, B -> False  , C -> True   )) == Unknown)
    assert(expr.eval(Map(A -> Unknown, B -> Unknown, C -> False  )) == Unknown)
    assert(expr.eval(Map(A -> Unknown, B -> Unknown, C -> Unknown)) == Unknown)
    assert(expr.eval(Map(A -> Unknown, B -> Unknown, C -> True   )) == Unknown)
    assert(expr.eval(Map(A -> Unknown, B -> True   , C -> False  )) == Unknown)
    assert(expr.eval(Map(A -> Unknown, B -> True   , C -> Unknown)) == Unknown)
    assert(expr.eval(Map(A -> Unknown, B -> True   , C -> True   )) == True)

    assert(expr.eval(Map(A -> True   , B -> False  , C -> False  )) == True)
    assert(expr.eval(Map(A -> True   , B -> False  , C -> Unknown)) == True)
    assert(expr.eval(Map(A -> True   , B -> False  , C -> True   )) == True)
    assert(expr.eval(Map(A -> True   , B -> Unknown, C -> False  )) == True)
    assert(expr.eval(Map(A -> True   , B -> Unknown, C -> Unknown)) == True)
    assert(expr.eval(Map(A -> True   , B -> Unknown, C -> True   )) == True)
    assert(expr.eval(Map(A -> True   , B -> True   , C -> False  )) == True)
    assert(expr.eval(Map(A -> True   , B -> True   , C -> Unknown)) == True)
    assert(expr.eval(Map(A -> True   , B -> True   , C -> True   )) == True)

  "'A'||'B'&&'C'" in:
    testParse("'A'||'B'&&'C'",
      Or(
        ExpectNotice(BoardPath("A")),
        And(
          ExpectNotice(BoardPath("B")),
          ExpectNotice(BoardPath("C")))))

  "'A' && ('B' || 'C')" in:
    val expr = testParse(
      "'A' && ('B' || 'C')",
      And(
        ExpectNotice(BoardPath("A")),
        Or(
          ExpectNotice(BoardPath("B")),
          ExpectNotice(BoardPath("C")))))

    assert(expr.eval(Map(A -> False  , B -> False  , C -> False  )) == False)
    assert(expr.eval(Map(A -> False  , B -> False  , C -> Unknown)) == False)
    assert(expr.eval(Map(A -> False  , B -> False  , C -> True   )) == False)
    assert(expr.eval(Map(A -> False  , B -> Unknown, C -> False  )) == False)
    assert(expr.eval(Map(A -> False  , B -> Unknown, C -> Unknown)) == False)
    assert(expr.eval(Map(A -> False  , B -> Unknown, C -> True   )) == False)
    assert(expr.eval(Map(A -> False  , B -> True   , C -> False  )) == False)
    assert(expr.eval(Map(A -> False  , B -> True   , C -> Unknown)) == False)
    assert(expr.eval(Map(A -> False  , B -> True   , C -> True   )) == False)

    assert(expr.eval(Map(A -> Unknown, B -> False  , C -> False  )) == False)
    assert(expr.eval(Map(A -> Unknown, B -> False  , C -> Unknown)) == Unknown)
    assert(expr.eval(Map(A -> Unknown, B -> False  , C -> True   )) == Unknown)
    assert(expr.eval(Map(A -> Unknown, B -> Unknown, C -> False  )) == Unknown)
    assert(expr.eval(Map(A -> Unknown, B -> Unknown, C -> Unknown)) == Unknown)
    assert(expr.eval(Map(A -> Unknown, B -> Unknown, C -> True   )) == Unknown)
    assert(expr.eval(Map(A -> Unknown, B -> True   , C -> False  )) == Unknown)
    assert(expr.eval(Map(A -> Unknown, B -> True   , C -> Unknown)) == Unknown)
    assert(expr.eval(Map(A -> Unknown, B -> True   , C -> True   )) == Unknown)

    assert(expr.eval(Map(A -> True   , B -> False  , C -> False  )) == False)
    assert(expr.eval(Map(A -> True   , B -> False  , C -> Unknown)) == Unknown)
    assert(expr.eval(Map(A -> True   , B -> False  , C -> True   )) == True)
    assert(expr.eval(Map(A -> True   , B -> Unknown, C -> False  )) == Unknown)
    assert(expr.eval(Map(A -> True   , B -> Unknown, C -> Unknown)) == Unknown)
    assert(expr.eval(Map(A -> True   , B -> Unknown, C -> True   )) == True)
    assert(expr.eval(Map(A -> True   , B -> True   , C -> False  )) == True)
    assert(expr.eval(Map(A -> True   , B -> True   , C -> Unknown)) == True)
    assert(expr.eval(Map(A -> True   , B -> True   , C -> True   )) == True)

  "'FOLDER/B_BOARD.2'" in:
    assert(parse("'FOLDER/B_BOARD.2'") == Right(
      ExpectNotice(BoardPath("FOLDER/B_BOARD.2"))))

  private def testParse(exprString: String, expr: BoardPathExpression)
    (implicit pos: source.Position)
  : BoardPathExpression =
    assert(parse(exprString) == Right(expr))
    assert(parse(expr.toString) == Right(expr), " - toString")
    parse(exprString).toOption.get
