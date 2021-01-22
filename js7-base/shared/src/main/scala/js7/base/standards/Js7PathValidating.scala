package js7.base.standards

import js7.base.generic.GenericString
import js7.base.generic.GenericString.{Checked_, EmptyStringProblem}
import js7.base.problem.Checked
import js7.base.problem.Problems.InvalidNameProblem

trait Js7PathValidating[A <: GenericString] extends Checked_[A]
{
  private lazy val nameValidator = new Js7PathValidator(name)

  override def checked(string: String): Checked[A] =
    if (string.isEmpty)
      EmptyStringProblem(name)
    else
      nameValidator.checked(string)
        .left.map {
          case InvalidNameProblem(`name`, _) => InvalidNameProblem(name, string)
          case EmptyStringProblem(`name`) => InvalidNameProblem(name, string)
          case o => o
        }
        .flatMap(super.checked)
}
