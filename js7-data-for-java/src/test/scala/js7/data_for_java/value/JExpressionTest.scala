package js7.data_for_java.value

import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.FastparseExpressionParser.parseExpression
import org.scalatest.freespec.AnyFreeSpec

final class JExpressionTest extends AnyFreeSpec
{
  "quoteString" in {
    def check(string: String, quotedString: String) = {
      assert(JExpression.quoteString(string) == quotedString)
      assert(parseExpression(quotedString) == Right(StringConstant(string)))
    }
    check("", "\"\"")  // ""
    check("|'$\"\n|", "\"|'\\$\\\"\\n|\"")  // "|'\$\"
    check("|'|", "\"|'|\"")  // "|'|"
    check("\\\"$\t\n", "'\\\"$\t\n'")
    check("\\\"$\t\r\n", """"\\\"\$\t\r\n"""") // "\\\"\$\t\r\n"
    check("'$NOVAR", """"'\$NOVAR"""")  // "'\$NOVAR"
    check("$NOVAR", "'$NOVAR'")  // '$NOVAR'
    check("|A|", "'|A|'")
    check("|\n|", "'|\n|'")
    check("|\t|", "'|\t|'")
    check("|\r\n|", """"|\r\n|"""")  // ", because ' normalizes \r\n to \n
  }
}
