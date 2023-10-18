package js7.base.standards

import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Checked
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.standards.Js7PathValidatorTest.checkInvalid
import js7.base.test.OurTestSuite
import org.scalatest.Assertions.*

final class Js7PathValidatorTest extends OurTestSuite:
  private val validator = new Js7PathValidator("TEST")
  import validator.checked

  "Invalid paths" in:
    checkInvalid("TEST", validator.checked)

  "Valid paths" in:
    assert(checked("A") == Right("A"))
    assert(checked("🔷") == Right("🔷"))
    assert(checked("A-B").isRight)
    assert(checked("A--B---C").isRight)
    assert(checked("folder/a-b").isRight)
    assert(checked("поток/워크플로").isRight)
    assert(checked("A-").isRight)
    assert(checked("A1").isRight)
    assert(checked("A-1").isRight)
    assert(checked("A.1").isRight)
    assert(checked("1").isRight)
    assert(checked("1A").isRight)


object Js7PathValidatorTest:
  def checkInvalid[A](typeName: String, checked: String => Checked[A]): Unit =
    assert(checked("") == Left(EmptyStringProblem(typeName)))
    assert(checked("/") == Left(InvalidNameProblem(typeName, "/")))
    assert(checked("/A") == Left(InvalidNameProblem(typeName, "/A")))
    assert(checked("A/") == Left(InvalidNameProblem(typeName, "A/")))
    assert(checked("A//B") == Left(InvalidNameProblem(typeName, "A//B")))
    assert(checked("A.").isLeft)
    assert(checked("A B").isLeft)
    assert(checked("A?") == Left(InvalidNameProblem(typeName, "A?")))
    assert(checked("A!") == Left(InvalidNameProblem(typeName, "A!")))
    assert(checked("?/xx") == Left(InvalidNameProblem(typeName, "?/xx")))
    assert(checked("/folder/a-b") == Left(InvalidNameProblem(typeName, "/folder/a-b")))
    assert(checked("a@b") == Left(InvalidNameProblem(typeName, "a@b")))
    assert(checked("a,b") == Left(InvalidNameProblem(typeName, "a,b")))
    assert(checked("a~b") == Left(InvalidNameProblem(typeName, "a~b")))  // "~" is used for VersionId

  def checkValid[A](checked: String => Checked[A]): Unit =
    assert(checked("A").isRight)
    assert(checked("🔷").isRight)
    assert(checked("A-B").isRight)
    assert(checked("A--B---C").isRight)
    assert(checked("folder/a-b").isRight)
    assert(checked("поток/워크플로").isRight)
    assert(checked("A-").isRight)
    assert(checked("A1").isRight)
    assert(checked("A-1").isRight)
    assert(checked("A.1").isRight)
    assert(checked("1").isRight)
    assert(checked("1A").isRight)
