package js7.data.value.expression.scopes

import cats.effect.{IO, Resource, ResourceIO}
import js7.base.problem.Checked
import js7.data.Problems.InvalidFunctionArgumentsProblem
import js7.data.value.StringValue
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.scopes.FileValueScope.*
import js7.data.value.expression.{Expression, Scope}

private[js7] final class FileValueScope private(
  private[scopes] val fileValueState: FileValueState)
extends Scope, AutoCloseable:

  fileValueState.startScope(this)

  override def evalFunctionCall(functionCall: Expression.FunctionCall)(implicit scope: Scope) =
    functionCall match
      case FunctionCall(`functionName`, arguments) =>
        Some(arguments match {
          case Seq(
            Argument(contentExpr, None | Some("content"))) =>
            toFile(contentExpr, None)

          case Seq(
            Argument(contentExpr, None | Some("content")),
            Argument(filenameExpr, None | Some("filename"))) =>
            toFile(contentExpr, Some(filenameExpr))

          case _ =>
            Left(InvalidFunctionArgumentsProblem(functionCall))
        })

      case _ =>
        super.evalFunctionCall(functionCall)

  private def toFile(contentExpr: Expression, filenameExpr: Option[Expression])
    (implicit scope: Scope)
  : Checked[StringValue] =
    for
      content <- contentExpr.evalAsString
      filenamePattern <- toFilenamePattern(filenameExpr)
      file <- fileValueState.toFile(this, filenamePattern, content)
    yield StringValue(file.toString)

  private def toFilenamePattern(filenameExpr: Option[Expression])(implicit scope: Scope)
  : Checked[String] =
    filenameExpr match
      case None =>
        Right("*")

      case Some(filenameExpr) =>
        filenameExpr.evalAsString

  def close(): Unit =
    fileValueState.releaseScope(this)


object FileValueScope:
  val functionName = "toFile"

  def resource(fileValueState: FileValueState): ResourceIO[FileValueScope] =
    Resource.fromAutoCloseable(IO:
      new FileValueScope(fileValueState))
