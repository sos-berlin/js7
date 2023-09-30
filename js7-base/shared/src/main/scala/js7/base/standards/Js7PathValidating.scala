package js7.base.standards

import js7.base.generic.GenericString
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Checked
import js7.base.problem.Problems.InvalidNameProblem

trait Js7PathValidating[A <: GenericString]
extends GenericString.Checked_[A]:

  private lazy val pathValidator = new Js7PathValidator(name)

  final def isNameStart(c: Char) =
    pathValidator.isNameStart(c)

  final def isNamePartMaybe(c: Char) =
    pathValidator.isNamePartMaybe(c)

  override def checked(string: String): Checked[A] =
    if string.isEmpty then
      EmptyStringProblem(name)
    else
      pathValidator.checked(string)
        .left.map:
          case InvalidNameProblem(`name`, _) => InvalidNameProblem(name, string)
          case EmptyStringProblem(`name`) => InvalidNameProblem(name, string)
          case o => o
        .flatMap(super.checked)
