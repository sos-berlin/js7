package js7.data.value.expression

trait Precedence:
  protected def precedence: Int

  protected def inParentheses(o: Precedence): String = Precedence.inParentheses(o, precedence)

  protected def makeString(a: Precedence, op: String, b: Precedence): String =
    Precedence.toString(a, op, precedence, b)


object Precedence:
  // Higher number means higher precedence
  private val next = Iterator.from(1).next _
  val Function: Int = next()
  val WordOperator: Int = next()
  val Or: Int = next()
  val And: Int = next()
  val Comparison: Int = next()
  val Equal: Int = next()
  val Addition: Int = next()
  val Multiplication: Int = next()
  val OrElse: Int = next()
  val Dot: Int = next()
  val Factor: Int = next()
  val Highest: Int = next()

  def toString(a: Precedence, op: String, opPrecedence: Int, b: Precedence): String =
    inParentheses(a, opPrecedence) + " " + op + " " + inParentheses(b, opPrecedence + 1)

  def inParentheses(o: Precedence, opPrecedence: Int): String =
    if o.precedence >= opPrecedence then
      o.toString
    else
      s"($o)"
