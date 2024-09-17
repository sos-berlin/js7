package js7.data.value.expression

trait HasPrecedence:
  protected def precedence: Int

  protected def inParentheses(o: HasPrecedence): String =
    HasPrecedence.inParentheses(o, precedence)

  protected def makeString(a: HasPrecedence, op: String, b: HasPrecedence): String =
    Precedence.toString(a, op, precedence, b)


object HasPrecedence:
  def inParentheses(o: HasPrecedence, opPrecedence: Int): String =
    if o.precedence < opPrecedence then
      s"($o)"
    else
      o.toString


object Precedence:
  // Higher number means higher precedence
  private val next = Iterator.from(1).next _
  val IfThenElse: Int = next()
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

  def toString(a: HasPrecedence, op: String, opPrecedence: Int, b: HasPrecedence): String =
    HasPrecedence.inParentheses(a, opPrecedence) + " " + 
      op + " " +
      HasPrecedence.inParentheses(b, opPrecedence + 1)

