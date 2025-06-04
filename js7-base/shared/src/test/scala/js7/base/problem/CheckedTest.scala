package js7.base.problem

import cats.effect.{IO, SyncIO}
import cats.instances.either.*
import cats.instances.int.*
import cats.instances.list.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.{Applicative, Apply}
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.CirceUtils.*
import js7.base.generic.Completed
import js7.base.problem.Checked.*
import js7.base.test.OurAsyncTestSuite
import js7.tester.CirceJsonTester.testJson
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class CheckedTest extends OurAsyncTestSuite:

  "JSON" - {
    import Checked.implicits.{checkedJsonDecoder, checkedJsonEncoder}

    case class A(number: Int)
    implicit val aJsonCodec: Codec.AsObject[A] = deriveCodec

    "Valid" in:
      testJson[Checked[A]](Right(A(1)),
        json"""{
          "number": 1
        }""")

    "Invalid" in:
      testJson[Checked[A]](Left(Problem("PROBLEM")),
        json"""{
          "TYPE": "Problem",
          "message": "PROBLEM"
        }""")
  }

  "===" in:
    assert(Checked(1) === Checked(1))
    assert(Checked(Problem("X")) === Checked(Problem.fromThrowable(new IllegalArgumentException("X"))))

  "fromOption" in:
    assert(Checked.fromOption(1.some, Problem("PROBLEM")) == Right(1))
    assert(Checked.fromOption(none, Problem("PROBLEM")) == Left(Problem("PROBLEM")))

  "flattenTryChecked" in:
    assert(Checked.flattenTryChecked(Success(Right(1))) == Right(1))
    assert(Checked.flattenTryChecked(Success(Left(Problem("PROBLEM")))) == Left(Problem("PROBLEM")))
    val throwable = new RuntimeException("EXCEPTION")
    assert(Checked.flattenTryChecked(Failure(throwable)) == Left(Problem("EXCEPTION")))

  "fromTry" in:
    assert(Checked.fromTry(Success(1)) == Right(1))
    val throwable = new RuntimeException("EXCEPTION")
    assert(Checked.fromTry(Failure(throwable)) == Left(Problem("EXCEPTION")))

  "check" in:
    assert(Checked.check(false, 7, Problem("PROBLEM")) == Left(Problem("PROBLEM")))
    assert(Checked.check(true, 7, Problem("PROBLEM")) == Right(7))

  "catchNonFatal" in:
    assert(catchNonFatal(7) == Right(7))

    val t = new IllegalArgumentException("TEST")
    val t2: Throwable = catchNonFatal(throw t).swap.toOption.get.throwable
    assert(t2 eq t)

  "catchExpected" in:
    assert((catchExpected[IllegalArgumentException](7): Checked[Int]) == Right(7))
    val t = new IllegalArgumentException("TEST")

    val checked = catchExpected[IllegalArgumentException](throw t)
    assert(checked.swap.toOption.get.throwable eq t)

    intercept[IllegalArgumentException]:
      catchExpected[IllegalStateException](throw t)
    succeed

  "catchProblem" in:
    assert(Checked.catchProblem(7) == Right(7))

    val t = new IllegalArgumentException("TEST")
    intercept[IllegalArgumentException] { Checked.catchProblem(throw t) }

    val problem = Problem("TEST")
    assert(Checked.catchProblem(throw problem.throwable).swap.getOrElse(null) eq problem)

  private val right1: Checked[Int] = Right(1)
  private val right2: Checked[Int] = Right(2)
  private val leftX: Checked[Int] = Left(Problem("X"))
  private val leftY: Checked[Int] = Left(Problem("Y"))

  "flatMap" in:
    assert((Left(Problem("A")): Checked[String]).flatMap((_: String) => throw new NotImplementedError) == Left(Problem("A")))
    assert((Right("A"): Checked[String]).flatMap(_ => Right(2)) == Right(2))

  "for-comprehension (flatMap)" in:
    val valid = for
      a <- right1
      b <- right2
    yield a -> b
    assert(valid == Right(1 -> 2))

  "for-comprehension (flatMap), fail-fast" in:
    val left = for
      a <- right1
      b <- leftX
      c <- right2
      d <- leftY
    yield (a, b, c, d)
    assert(left == Left(Problem("X")))

  "mapN" in:
    assert((right1, right2).mapN((a, b) => (a, b)) == Right(1 -> 2))

  //"mapN combines Problems" in {
  //  assert((right1, leftX, right2, leftY).mapN((_, _, _, _) => ()) == Left(Problem("X\n & Y")))
  //}

  "mapN fails-fast" in:
    assert((right1, leftX, right2, leftY).mapN((_, _, _, _) => ()) == Left(Problem("X")))

  "withProblemKey" in:
    assert(Right(1).withProblemKey("X") == Right(1))
    assert(Left(Problem("X")).withProblemKey(333) == Left(Problem("Problem with '333': X")))

  "mapProblem" in:
    assert(Right(1).mapProblem(_ => throw new NotImplementedError) == Right(1))
    assert(Left(Problem("X")).mapProblem(p => Problem(s"/$p/")) == Left(Problem("/X/")))

  "onProblem" in:
    assert(Right(1).onProblem(_ => throw new NotImplementedError) == 1.some)
    var flag = false
    assert(Left(Problem("X")).onProblem(_ => flag = true) == none)
    assert(flag)

  "onProblemHandle" in:
    assert(Right(1).onProblemHandle(_ => throw new NotImplementedError) == 1)
    var flag = false
    assert(Left(Problem("X")).onProblemHandle { _ => flag = true; 7 } == 7)
    assert(flag)

  "handleProblem in IO" in:
    val left: Checked[Int] = Left(Problem("PROBLEM"))
    val right: Checked[Int] = Right(1)
    IO(left).handleProblem(problem => problem.toString).map: x =>
      assert(x == "PROBLEM")
    IO(right).handleProblem(problem => problem.toString).map: x =>
      assert(x == 1)

  "handleProblemWith in IO" in:
    val left: Checked[Int] = Left(Problem("PROBLEM"))
    val right: Checked[Int] = Right(1)
    IO(left).handleProblemWith(problem => IO(problem.toString)).map: x =>
      assert(x == "PROBLEM")
    IO(right).handleProblemWith(problem => IO(problem.toString)).map: x =>
      assert(x == 1)

  "recoverFromProblem in IO" in:
    val left1: Checked[Int] = Left(Problem("PROBLEM"))
    val left2: Checked[Int] = Left(Problem("PROBLEM-2"))
    val right: Checked[Int] = Right(1)

    IO(left1).recoverFromProblem:
      case problem if problem == Problem("PROBLEM") => problem.toString
    .map: x =>
      assert(x == Right("PROBLEM"))

    IO(left2).recoverFromProblem:
      case problem if problem == Problem("PROBLEM") => problem.toString
    .map: x =>
      assert(x == Left(Problem("PROBLEM-2")))

  "recoverFromProblemWith in IO" in:
    val left1: Checked[Int] = Left(Problem("PROBLEM"))
    val left2: Checked[Int] = Left(Problem("PROBLEM-2"))
    val right: Checked[Int] = Right(1)

    IO(left1).recoverFromProblemWith:
      case problem if problem == Problem("PROBLEM") => IO(Right(problem.toString))
    .map: x =>
      assert(x == Right("PROBLEM"))

    IO(left2).recoverFromProblemWith:
      case problem if problem == Problem("PROBLEM") => IO(Right(problem.toString))
    .map: x =>
      assert(x == Left(Problem("PROBLEM-2")))

  "recoverFromProblemWith in IO (2)" in :
    val ok: IO[Checked[Int]] = IO.pure(Right(1))
    val failed: IO[Checked[Int]] = IO.pure(Left(TestCodeProblem))
    for
      x <- ok.recoverFromProblemWith:
        case TestCodeProblem => IO.pure(Right(999))
      _ = assert(x == Right(1))
      x <- failed.recoverFromProblemWith:
        case TestCodeProblem if false => IO.pure(Right(2))
      _ = assert(x == Left(TestCodeProblem))
      x <- failed.recoverFromProblemWith:
        case TestCodeProblem => IO.pure(Right(2))
      _ = assert(x == Right(2))
      x <- failed.recoverFromProblemWith:
        case TestCodeProblem => IO.pure(Left(Problem("PROBLEM")))
      _ = assert(x == Left(Problem("PROBLEM")))
    yield
      succeed

  "traverse with Checked" in:
    def check(i: Int): Checked[String] = if (i % 2) == 0 then Right(i.toString) else Left(Problem(s"odd $i"))
    assert(Left(Problem("PROBLEM")).traverse(check) == Right(Left(Problem("PROBLEM"))))
    assert(Right(1).traverse(check) == Left(Problem("odd 1")))
    assert(Right(2).traverse(check) == Right(Right("2")))

  "traverse with List" in:
    def toList(n: Int) = (1 to n).toList
    assert(Left(Problem("PROBLEM")).traverse(toList) == Left(Problem("PROBLEM")) :: Nil)
    assert(Right(2).traverse(toList) == Right(1) :: Right(2) :: Nil)

  "traverse with SyncIO" in:
    def toSyncIO(n: Int) = SyncIO(n)

    val a: SyncIO[Checked[Int]] = Left(Problem("PROBLEM")).traverse(toSyncIO)
    assert(a.unsafeRunSync() == Left(Problem("PROBLEM")))

    val b: SyncIO[Checked[Int]] = Right(1).traverse(toSyncIO)
    assert(b.unsafeRunSync() == Right(1))

  "traverse a List" in:
    def check(i: Int): Checked[String] = if (i % 2) == 0 then Right(i.toString) else Left(Problem(s"odd $i"))
    //assert(List(1, 2, 3).traverse(check) == Left(Problem.multiple("odd 1", "odd 3")))
    assert(List(1, 2, 3).traverse(check) == Left(Problem("odd 1")))
    assert(List(2, 4, 6).traverse(check) == Right(List("2", "4", "6")))

  "sequence" in:
    assert(List(Right(1), Right(2), Right(3)).sequence[Checked, Int] == Right(List(1, 2, 3)))
  //assert(List(Right(1), Left(Problem("X")), Left(Problem("Y"))).sequence[Checked, Int] == Left(Problem.multiple("X", "Y")))
    assert(List(Right(1), Left(Problem("X")), Left(Problem("Y"))).sequence[Checked, Int] == Left(Problem("X")))

  "valueOr" in:
    assert(Right(1).orElse(sys.error("???")) == Right(1))
    assert(Left(Problem("PROBLEM")).orElse(Right(1)) == Right(1))
    assert(Left(Problem("FIRST")).orElse(Left(Problem("SECOND"))) == Left(Problem("SECOND")))

  "orElse" in:
    assert(Right(1).orElse(sys.error("???")) == Right(1))
    assert(Left(Problem("PROBLEM")).orElse(Right(1)) == Right(1))
    assert(Left(Problem("FIRST")).orElse(Left(Problem("SECOND"))) == Left(Problem("SECOND")))

  "toUnit" in:
    assert(Left(Problem("PROBLEM")).toUnit == Left(Problem("PROBLEM")))
    assert(Right(1).toUnit == Right(()))

  "toCompleted" in:
    assert(Left(Problem("PROBLEM")).toCompleted == Left(Problem("PROBLEM")))
    assert(Right(1).toCompleted == Right(Completed))

  "orThrow" in:
    assert(Right(1).orThrow == 1)
    assert(intercept[ProblemException](Left(Problem("PROBLEM")).orThrow).getMessage == "PROBLEM")

  "toEitherThrowable" in:
    assert(Checked(1).toEitherThrowable == Right(1))
    val Left(throwable: Throwable) = Left(Problem("PROBLEM")).toEitherThrowable: @unchecked
    assert(throwable.getMessage == "PROBLEM")

  "asTry" in:
    assert(Checked(1).asTry == Success(1))
    val Failure(throwable: Throwable) = Left(Problem("PROBLEM")).asTry: @unchecked
    assert(throwable.getMessage == "PROBLEM")

  "traverseAndCombineProblems" in:
    assert(List(Right(1), Right(2)).traverseAndCombineProblems(i => Right(i * 11)) == Right(Seq(11, 22)))
    assert(List(Left(Problem("ONE")), Right(2), Left(Problem("TWO"))).traverseAndCombineProblems(i => Right(i * 11)) ==
      Left(Problem.combined(List("ONE", "TWO"))))

  "combineProblems" in:
    assert(List(Right(1), Right(2)).combineProblems == Right(Seq(1, 2)))
    assert(List(Left(Problem("ONE")), Right(2), Left(Problem("TWO"))).combineProblems ==
      Left(Problem.combined(List("ONE", "TWO"))))

  "failFastMap" - {
    "Invalid" in:
      var lastI = 0
      def f(i: Int) =
        lastI = i
        if i % 2 == 0 then Right(i) else Left(Problem("ODD"))
      assert(List(2, 3, 4).failFastMap(f) == Left(Problem("ODD")))
      assert(lastI == 3)

    "Valid" in:
      def g(i: Int): Checked[Int] = Right(i * 11)
      assert(List(1, 2, 3).failFastMap(g) == Right(List(11, 22, 33)))
  }

  "Monoid[Checked]" in:
    assert(Seq(1, 2, 3).combineAll == 6)
    assert(Seq[Checked[Int]]().combineAll == Right(0))
    assert(Seq[Checked[Int]](Right(1), Right(2), Right(3)).combineAll == Right(6))
    assert(Seq[Checked[Int]](Right(1), Left(Problem("A")), Right(3), Left(Problem("B")))
      .combineAll == Left(Problem("A") |+| Problem("B")))

  "Some Cats examples" - {
    val valid1: Checked[Int] = Right(1)
    val valid2: Checked[Int] = Right(2)
    val problemA = Problem("A")
    val problemB = Problem("B")
    val problemC = Problem("C")
    val invalidA: Checked[Int] = Left(problemA)
    val invalidB: Checked[Int] = Left(problemB)

    "product" in:
      assert(Applicative[Checked].product(valid1, valid2) == Right((1, 2)))
      assert(Applicative[Checked].product(valid1, problemB) == Left(problemB))
      assert(Applicative[Checked].product(problemA, valid2) == Left(problemA))
      assert(Applicative[Checked].product(problemA, problemB) == Left(problemA))
      assert(Apply      [Checked].product(problemA, problemB) == Left(problemA))

    "productR" in:
      assert(Applicative[Checked].productR(valid1)(valid2) == valid2)
      assert(Applicative[Checked].productR(valid1)(problemB) == Left(problemB))
      assert(Applicative[Checked].productR(problemA)(valid2) == Left(problemA))
      assert(Applicative[Checked].productR(problemA)(problemB) == Left(problemA))
      assert(Apply      [Checked].productR(problemA)(problemB) == Left(problemA))

    "*> (productR)" in:
      assert((valid1 *> valid2) == valid2)
      assert((valid1 *> invalidB) == invalidB)
      assert((invalidA *> valid2) == invalidA)
      assert((invalidA *> invalidB) == Left(problemA))

    "ap" in:
      val ff: Checked[Int => String] = Right(_.toString)
      assert(Applicative[Checked].ap(ff)(valid1) == Right("1"))
      assert(Applicative[Checked].ap(ff)(problemB) == Left(problemB))
      assert(Applicative[Checked].ap(problemA)(valid1) == Left(problemA))
      assert(Applicative[Checked].ap(problemA)(problemB) == Left(problemA))
      assert(Apply      [Checked].ap(problemA)(problemB) == Left(problemA))

    "<*> (ap)" in:
      val validFf: Checked[Int => String] = Right(_.toString)
      val invalidFf: Checked[Int => String] = Problem("ff")
      assert((validFf <*> valid1) == Right("1"))
      assert((validFf <*> invalidB) == Left(Problem("B")))
      assert((invalidFf <*> valid1) == Left(Problem("ff")))
      assert((invalidFf <*> invalidB) == Left(Problem("ff")))

    "ap2" in:
      val ff: Checked[(Int, Int) => String] = Right(_.toString + _.toString)
      assert(Applicative[Checked].ap2(ff)(valid1, valid2) == Right("12"))
      assert(Applicative[Checked].ap2(ff)(valid1, problemB) == Left(problemB))
      assert(Applicative[Checked].ap2(ff)(problemA, valid2) == Left(problemA))
      assert(Applicative[Checked].ap2(ff)(problemA, problemB) == Left(problemA))
      assert(Applicative[Checked].ap2(problemC)(valid1, valid2) == Left(problemC))
      assert(Applicative[Checked].ap2(problemC)(valid1, problemB) == Left(problemB))
      assert(Applicative[Checked].ap2(problemC)(problemA, problemB) == Left(problemA))
      assert(Apply      [Checked].ap2(problemC)(problemA, problemB) == Left(problemA))

    "map2" in:
      def f(a: Int, b: Int) = a.toString + b.toString
      assert(Applicative[Checked].map2(valid1, valid2)(f) == Right("12"))
      assert(Applicative[Checked].map2(valid1, problemB)(f) == Left(problemB))
      assert(Applicative[Checked].map2(problemA, valid2)(f) == Left(problemA))
      assert(Applicative[Checked].map2(problemA, problemB)(f) == Left(problemA))
      assert(Apply      [Checked].map2(problemA, problemB)(f) == Left(problemA))
  }
