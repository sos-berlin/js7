package com.sos.jobscheduler.base.problem

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import java.util.Optional

/**
  * @author Joacim Zschimmer
  */
final case class JavaChecked[A](checked: Checked[A])
{
  def isValid = checked.isValid

  def isInvalid = checked.isInvalid

  @throws[ProblemException]
  def get: A =
    try checked.orThrow
    catch { // Don't throw undeclared unchecked exception
      case t @ (_: RuntimeException | _: Error) ⇒ throw t
      case t ⇒ throw new RuntimeException(t.toSimplifiedString, t)
    }

  def toOptional: Optional[A] =
    checked match {
      case Valid(a) ⇒ Optional.of(a)
      case Invalid(_) ⇒ Optional.empty[A]
    }

  def problem: Optional[Problem] =
    checked match {
      case Valid(_) ⇒ Optional.empty[Problem]
      case Invalid(problem) ⇒ Optional.of(problem)
    }
}

object JavaChecked {
  //implicit final class ToJavaChecked[A](val underlying: Checked[A]) extends AnyVal {
  //  def toJava = JavaChecked(underlying)
  //}
}
