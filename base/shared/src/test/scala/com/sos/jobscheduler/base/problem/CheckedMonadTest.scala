package com.sos.jobscheduler.base.problem

import cats.data.Validated.{Invalid, Valid}
import cats.instances.int._
import cats.instances.tuple._
import cats.laws.discipline.MonadTests
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import com.sos.jobscheduler.base.problem.Checked.{_eq, monad}
import com.sos.jobscheduler.base.problem.CheckedMonadTest._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

/**
  * @author Joacim Zschimmer
  */
final class CheckedMonadTest extends FunSuite with Discipline
{
  private implicit def arbitraryChecked[T](implicit a: Arbitrary[T]): Arbitrary[Checked[T]] =
    Arbitrary(Gen.oneOf(
      arbitrary[T] map Valid.apply,
      Gen.const(TestInvalid)))

  private implicit val isomorphisms: Isomorphisms[Checked] = Isomorphisms.invariant(Checked.monad)

  checkAll("Checked[Int]", MonadTests[Checked].monad[Int, Int, Int])
}

object CheckedMonadTest {
  private val TestInvalid = Invalid(Problem("INVALID"))
}
