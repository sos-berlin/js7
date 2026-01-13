package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.data.value.Value
import js7.data.value.expression.Expression.{FunctionCall, JobResourceVariable}
import js7.data.value.expression.{Scope, ValueSearch}
import scala.annotation.targetName

private[expression] trait CombinedScope extends Scope:

  protected def scopes: Seq[Scope]

  protected def find(f: Scope => Option[Checked[Value]]): Option[Checked[Value]]

  override def symbolValue(name: String): Option[Checked[Value]] =
    find(_.symbolValue(name))

  override def namedValue(name: String): Option[Checked[Value]] =
    find(_.namedValue(name))

  override def findValue(search: ValueSearch) =
    find(_.findValue(search))

  override def evalFunctionCall(functionCall: FunctionCall)(implicit scope: Scope) =
    find(_.evalFunctionCall(functionCall))

  override def evalJobResourceVariable(v: JobResourceVariable)(implicit scope: Scope) =
    find(_.evalJobResourceVariable(v))

  override def toString =
    s"${scopes.mkString(" |+| ")}" // combine is associative


object CombinedScope:

  def apply(first: Scope, second: Scope): CombinedScope =
    new PairCombinedScope(first, second)

  inline def apply(scopes: Seq[Scope]): CombinedScope =
    new SeqCombinedScope(scopes)

  @targetName("applyVarargs")
  inline def apply(inline scopes: Scope*): CombinedScope =
    new SeqCombinedScope(scopes)


  private final class PairCombinedScope(first: Scope, second: Scope)
  extends CombinedScope:

    protected def scopes: Seq[Scope] = first :: second :: Nil

    protected def find(f: Scope => Option[Checked[Value]]): Option[Checked[Value]] =
      f(first) orElse f(second)


  final class SeqCombinedScope(protected val scopes: Seq[Scope])
  extends CombinedScope:

    protected def find(f: Scope => Option[Checked[Value]]): Option[Checked[Value]] =
      scopes.view.flatMap(f).headOption
