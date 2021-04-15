package js7.base.standards

import cats.syntax.traverse._
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Checked
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.utils.ScalaUtils.syntax.RichEither

final class Js7PathValidator(val typeName: String) extends NameValidator
{
  private val js7NameValidator = new Js7NameValidator(typeName)

  override def checked(path: String): Checked[String] =
    if (path.isEmpty)
      Left(EmptyStringProblem(typeName))
    else
      path
        .split("/", -1)
        .toList
        .traverse(js7NameValidator.checked)
        .left.map {
          case EmptyStringProblem(`typeName`) => InvalidNameProblem(typeName, path)
          case InvalidNameProblem(`typeName`, _) => InvalidNameProblem(typeName, path)
          case o => o
        }
        .rightAs(path)
}
