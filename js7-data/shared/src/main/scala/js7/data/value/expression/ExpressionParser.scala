package js7.data.value.expression

import js7.base.problem.Checked

object ExpressionParser
{
  def expr(expressionString: String): Expression =
    CatsExpressionParser.expr(expressionString)

  def parseExpression(string: String): Checked[Expression] =
    CatsExpressionParser.parseExpression(string)

  def exprFunction(string: String): ExprFunction =
    CatsExpressionParser.exprFunction(string)

  def parseFunction(string: String): Checked[ExprFunction] =
    CatsExpressionParser.parseFunction(string)

  def parseExpressionOrFunction(string: String): Checked[Expression] =
    CatsExpressionParser.parseExpressionOrFunction(string)

  def parseQuotedString(quoted: String): Checked[String] =
    CatsExpressionParser.parseQuotedString(quoted)
}
