package com.sos.jobscheduler.base.problem

import cats.data.Validated.{Invalid, Valid}
import cats.instances.int._
import cats.instances.tuple._
import cats.laws.discipline.FlatMapTests
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import com.sos.jobscheduler.base.problem.Checked._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

/**
  * @author Joacim Zschimmer
  */
final class CheckedLawTest extends FunSuite with Discipline
{
  private implicit def arbitraryChecked[A: Arbitrary]: Arbitrary[Checked[A]] =
    Arbitrary(Gen.oneOf(
      arbitrary[A] map Valid.apply,
      Gen.const(Invalid(Problem("INVALID")))))

  private implicit val isomorphisms: Isomorphisms[Checked] = Isomorphisms.invariant(Checked.flatMap)

  checkAll("Checked[Int]", FlatMapTests[Checked].flatMap[Int, Int, Int])
}
