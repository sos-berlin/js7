package com.sos.jobscheduler.base.problem

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import java.util.Optional

/**
  * @author Joacim Zschimmer
  */
final case class JavaChecked[A](checked: Checked[A])
{
  def isValid = checked.isRight

  def isInvalid = checked.isLeft

  @throws[ProblemException]
  def get: A =
    try checked.orThrow
    catch { // Don't throw undeclared unchecked exception
      case t @ (_: RuntimeException | _: Error) => throw t
      case t: Throwable => throw new RuntimeException(t.toSimplifiedString, t)
    }

  def toOptional: Optional[A] =
    checked match {
      case Right(a) => Optional.of(a)
      case Left(_) => Optional.empty[A]
    }

  def problem: Optional[Problem] =
    checked match {
      case Right(_) => Optional.empty[Problem]
      case Left(problem) => Optional.of(problem)
    }
}

object JavaChecked {
  //implicit final class ToJavaChecked[A](val underlying: Checked[A]) extends AnyVal {
  //  def toJava = JavaChecked(underlying)
  //}
}
