package js7.data.board

import js7.data.board.BoardPathExpression.{And, ExpectNotice, Or}
import js7.data.board.BoardPathExpressionParser.parse
import org.scalatest.freespec.AnyFreeSpec

final class BoardPathExpressionTest extends AnyFreeSpec
{
  "A" in {
    assert(parse("'A'") == Right(ExpectNotice(BoardPath("A"))))
  }

  "'A' || 'B' && 'C'" in {
    assert(parse("'A' || 'B' && 'C'") == Right(
      Or(
        ExpectNotice(BoardPath("A")),
        And(
          ExpectNotice(BoardPath("B")),
          ExpectNotice(BoardPath("C"))))))
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
