package js7.data.value

import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.ObjectExpression

object ValuePrinter
{
  def nameToExpressionToString(nameToExpr: Map[String, Expression]): String = {
    val sb = new StringBuilder
    appendNameToExpression(sb, nameToExpr)
    sb.toString
  }

  def appendNameToExpression(sb: StringBuilder, nameToExpr: Map[String, Expression]): Unit = {
    sb.append('{')
    for ((name, expr) <- nameToExpr) {
      if (sb.last != '{') sb.append(", ")
      appendQuoted(sb, name)
      sb.append(": ")
      sb.append(expr.toString)
    }
    sb.append('}')
  }

  def appendValue(sb: StringBuilder, value: Value): Unit =
    value match {
      case BooleanValue(bool) => sb.append(bool)
      case NumberValue(number) => sb.append(number)
      case StringValue(string) => appendQuoted(sb, string)
      case ListValue(values) =>
        sb.append('[')
        val it = values.iterator
        if (it.hasNext) {
          appendValue(sb, it.next())
          while (it.hasNext) {
            sb.append(", ")
            appendValue(sb, it.next())
          }
        }
        sb.append(']')
    }

  def quoteString(string: String) = {
    val sb = new StringBuilder(string.length + 10)
    appendQuoted(sb, string)
    sb.toString
  }

  private val quotedChars = Set[Char]('\\', '\"', '$', '\n')

  def appendQuoted(sb: StringBuilder, string: String): Unit = {
    if (string.isEmpty) {
      sb.append("\"\"")
    } else if (!string.contains('\'') && string.exists(quotedChars)) {
      sb.append('\'')
      sb.append(string)
      sb.append('\'')
    } else {
      sb.append('"')
      appendQuotedContent(sb, string)
      sb.append('"')
    }
  }

  def appendQuotedContent(sb: StringBuilder, string: String): Unit =
    if (string.nonEmpty) {
      string foreach {
        case '\\' => sb.append("\\\\")
        case '"' => sb.append("\\\"")
        case '$' => sb.append("\\$")
        case '\r' => sb.append("\\r")
        case '\n' => sb.append("\\n")
        case '\t' => sb.append("\\t")
        case c => sb.append(c)
      }
    }
}
