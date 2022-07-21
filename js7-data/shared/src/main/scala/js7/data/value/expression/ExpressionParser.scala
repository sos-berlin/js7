package js7.data.value.expression

import js7.base.problem.Checked
import js7.data.parser.UseFastparse

object ExpressionParser
{
  def expr(expressionString: String): Expression =
    if (UseFastparse)
      FastparseExpressionParser.expr(expressionString)
    else
      CatsExpressionParser.expr(expressionString)

  def parseExpression(string: String): Checked[Expression] =
    if (UseFastparse)
      FastparseExpressionParser.parseExpression(string)
    else
      CatsExpressionParser.parseExpression(string)

  def exprFunction(string: String): ExprFunction =
    if (UseFastparse)
      FastparseExpressionParser.exprFunction(string)
    else
      CatsExpressionParser.exprFunction(string)

  def parseFunction(string: String): Checked[ExprFunction] =
    if (UseFastparse)
      FastparseExpressionParser.parseFunction(string)
    else
      CatsExpressionParser.parseFunction(string)

  def parseExpressionOrFunction(string: String): Checked[Expression] =
    if (UseFastparse)
      FastparseExpressionParser.parseExpressionOrFunction(string)
    else
      CatsExpressionParser.parseExpressionOrFunction(string)

  def parseQuotedString(quoted: String): Checked[String] =
    if (UseFastparse)
      FastparseExpressionParser.parseQuotedString(quoted)
    else
      CatsExpressionParser.parseQuotedString(quoted)
}
