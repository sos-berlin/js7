//package js7.base.monix
//
//import cats.laws.discipline.ApplicativeTests
//import cats.laws.discipline.SemigroupalTests.Isomorphisms
//import js7.base.monix.MonixForCats.*
//import monix.eval.Task
//import org.scalacheck.Arbitrary
//import org.scalacheck.Arbitrary.arbitrary
//import org.scalatest.FunSuite
//import org.typelevel.discipline.scalatest.Discipline
//
///**
//  * @author Joacim Zschimmer
//  */
//class MonixForCatsLawTest extends FunSuite with Discipline {
//
//  private implicit def arbitraryTask[A: Arbitrary]: Arbitrary[Task[A]] =
//    Arbitrary(arbitrary[A] map Task.pure)
//
//  private implicit val isomorphisms: Isomorphisms[Task] = Isomorphisms.invariant(taskApplicative)
//
//  checkAll("Task[Int]", ApplicativeTests[Task].applicative[Int, Int, Int])
//}
