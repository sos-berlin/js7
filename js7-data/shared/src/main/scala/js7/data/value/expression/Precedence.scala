package js7.data.value.expression

trait Precedence {
  protected def precedence: Int

  protected def inParentheses(o: Precedence): String = Precedence.inParentheses(o, precedence)

  protected def toString(a: Precedence, op: String, b: Precedence) =
    Precedence.toString(a, op, precedence, b)
}

object Precedence {
  // Higher number means higher precedence
  private val next = Iterator.from(1).next _
  val Function = next()
  val DotOperator = next()  // correct precedence ???
  val WordOperator = next()
  val Word1Operator = next()
  val Or = next()
  val And = next()
  val Comparison = next()
  val Addition = next()
  val Multiplication = next()
  val Factor = next()
  val Highest = next()

  def toString(a: Precedence, op: String, opPrecedence: Int, b: Precedence): String =
    inParentheses(a, opPrecedence) + " " + op + " " + inParentheses(b, opPrecedence + 1)

  def inParentheses(o: Precedence, opPrecedence: Int): String =
    if (o.precedence >= opPrecedence)
      o.toString
    else
      s"($o)"
}