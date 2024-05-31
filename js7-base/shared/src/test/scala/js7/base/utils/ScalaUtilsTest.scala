package js7.base.utils

import cats.effect.SyncIO
import cats.instances.either.*
import cats.instances.list.*
import cats.instances.option.*
import cats.syntax.option.*
import java.util.concurrent.atomic.AtomicBoolean
import js7.base.exceptions.StandardPublicException
import js7.base.log.Logger
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.RichDeadline
import js7.base.time.Stopwatch
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.ScalaUtilsTest.*
import org.scalatest.matchers.should.Matchers.*
import scala.collection.{MapView, View}
import scala.concurrent.duration.Deadline.now
import scala.reflect.ClassTag
import scala.util.Random
import scala.util.control.NoStackTrace

final class ScalaUtilsTest extends OurTestSuite:

  "Monad transforming funtions" - {
    def ignored = throw new RuntimeException("Nothing")
    type E = Either[String, Int]
    type O = Option[E]

    "rightAs" in:
      assert((None: O).rightAs(true) == None)
      assert((Some(Left("A")): O).rightAs(true) == Some(Left("A")))
      assert((Some(Right(7)): O).rightAs(true) == Some(Right(true)))
      assert((Some(Left("X")): O).rightAs(()) == Some(Left("X")))  // Optimized
      assert((Some(Right(7)): O).rightAs(()).get eq RightUnit)  // Optimized

    "mapt" in:
      assert((None: O).mapt(_ => ignored) == None)
      assert((Some(Left("A")): O).mapt(_ => ignored) == Some(Left("A")))
      assert((Some(Right(7)): O).mapt(3 * _) == Some(Right(21)))

    "mapT" in:
      assert((None: O).mapT(_ => ignored) == None)
      assert((Some(Left("A")): O).mapT(_ => Left("B")) == Some(Left("A")))
      assert((Some(Left("A")): O).mapT(o => Right(3 * o)) == Some(Left("A")))
      assert((Some(Right(7)): O).mapT(_ => Left("B")) == Some(Left("B")))
      assert((Some(Right(7)): O).mapT(o => Right(3 * o)) == Some(Right(21)))

    "flatMapT" - {
      "Option" in:
        type O = Option[E]
        assert(none[E].flatMapT(_ => ignored) == none[E])
        assert((Some(Left("A")): O).flatMapT(_ => ignored) == Some(Left("A")))
        assert((Some(Right(7)): O).flatMapT(_ => none[E]) == none[E])
        assert((Some(Right(7)): O).flatMapT(_ => Some(Left("B"))) == Some(Left("B")))
        assert((Some(Right(7)): O).flatMapT(o => Some(Right(3 * o))) == Some(Right(21)))

        // flatMapT with Option[Either[String, Nothing]] does not compile:
        //assert((Some(Left("A")): Option[Either[String, Nothing]]).flatMapT(_ => nothing) == Some(Left("A")))

      "orElseT" in:
        type E = Either[String, Option[Int]]
        assert((Left("A"): E).orElseT(Left("B")) == Left("A"))
        assert((Left("A"): E).orElseT(Right(None)) == Left("A"))
        assert((Left("A"): E).orElseT(Right(Some(2))) == Left("A"))

        assert((Right(None): E).orElseT(Left("B")) == Left("B"))
        assert((Right(None): E).orElseT(Right(None)) == Right(None))
        assert((Right(None): E).orElseT(Right(Some(2))) == Right(Some(2)))

        assert((Right(Some(1)): E).orElseT(Left("B")) == Right(Some(1)))
        assert((Right(Some(1)): E).orElseT(Right(None)) == Right(Some(1)))
        assert((Right(Some(1)): E).orElseT(Right(Some(2))) == Right(Some(1)))
    }

    "flatTapT" - {
      "Option" in:
        type O = Option[E]
        assert(none[E].flatMapT(_ => ignored) == none[E])
        assert((Some(Left("A")): O).flatTapT(_ => ignored) == Some(Left("A")))
        assert((Some(Right(7)): O).flatTapT(_ => none[E]) == none[E])
        assert((Some(Right(7)): O).flatTapT(_ => Some(Left("B"))) == Some(Left("B")))
        assert((Some(Right(7)): O).flatTapT(o => Some(Right(3 * o))) == Some(Right(7)))
    }

    "SyncIO" in:
      assert(SyncIO[E](Left("A")).flatMapT(_ => SyncIO.pure(Left("B"))).unsafeRunSync() == Left("A"))
      assert(SyncIO[E](Left("A")).flatMapT(right => SyncIO.pure(Right(3 * right))).unsafeRunSync() == Left("A"))
      assert(SyncIO[E](Right(7)).flatMapT(_ => SyncIO.pure(Left("B"))).unsafeRunSync() == Left("B"))
      assert(SyncIO[E](Right(7)).flatMapT(right => SyncIO.pure(Right(3 * right))).unsafeRunSync() == Right(21))

    "flatMapLeft" in:
      assert(SyncIO[E](Left("A")).flatMapLeft(left => SyncIO.pure(Left(left + "*"))).unsafeRunSync() == Left("A*"))
      assert(SyncIO[E](Left("9")).flatMapLeft(left => SyncIO.pure(Right(left.toInt))).unsafeRunSync() == Right(9))
      assert(SyncIO[E](Right(3)).flatMapLeft(_ => fail()).unsafeRunSync() == Right(3))

    "flatMapLeftCase" in:
      assert(SyncIO[E](Left("A")).flatMapLeftCase(left => SyncIO.pure(Left(left + "*"))).unsafeRunSync() == Left("A*"))
      assert(SyncIO[E](Left("9")).flatMapLeftCase(left => SyncIO.pure(Right(left.toInt))).unsafeRunSync() == Right(9))
      assert(SyncIO[E](Left("A")).flatMapLeftCase { case "A" => SyncIO.pure(Left("B")) }.unsafeRunSync() == Left("B"))
      assert(SyncIO[E](Left("X")).flatMapLeftCase { case "A" => SyncIO.pure(Left("B")) }.unsafeRunSync() == Left("X"))
      assert(SyncIO[E](Right(3)).flatMapLeftCase(_ => fail()).unsafeRunSync() == Right(3))
  }

  "reuseIfEqual" in:
    case class A(number: Int)
    val a = A(1)
    val b = A(1)
    val c = A(2)
    assert(a == b && a.ne(b))
    assert(reuseIfEqual(a, b) eq a)
    assert(reuseIfEqual(b, c) eq c)
    assert(reuseIfEqual(a)(_.copy(number = 7)) ne a)
    assert(reuseIfEqual(a)(_.copy(number = 1)) eq a)

  "reuseIfIdentical()" in:
    case class A(number: Int)
    val a = A(1)
    val b = A(1)
    val c = A(2)
    assert(a == b && a.ne(b))
    assert(reuseIfIdentical(a, b) eq b)
    assert(reuseIfIdentical(b, c) eq c)
    assert(reuseIfIdentical(a, a) eq a)
    assert(reuseIfIdentical(b, b) eq b)
    assert(reuseIfIdentical(a)(_.copy(number = 7)) ne a)
    assert(reuseIfIdentical(a)(_.copy(number = 1)) ne a)

  "implicitClass" in:
    def f[A : ClassTag] = implicitClass[A]
    f[String] shouldEqual classOf[String]

  "scalaName" in:
    assert(ScalaUtilsTest.getClass.getName == "js7.base.utils.ScalaUtilsTest$")
    assert(ScalaUtilsTest.getClass.scalaName == "js7.base.utils.ScalaUtilsTest")

  "scalaSimpleName" in:
    assert(ScalaUtilsTest.getClass.getSimpleName == "ScalaUtilsTest$")
    assert(ScalaUtilsTest.getClass.simpleScalaName == "ScalaUtilsTest")

  "simpleClassName" in:
    object A:
      object B:
        def getSimpleName = getClass.getSimpleName
        def simpleName = getClass.simpleName

    //scala-js does not know Java SourceVersion:
    //if (SourceVersion.values.map(_.toString) contains "RELEASE_9")
    //  assert(A.B.getSimpleName == "B$")
    //else
    //  intercept[java.lang.InternalError] {  // Until Java 8: https://bugs.openjdk.java.net/browse/JDK-8057919
    //    A.B.getSimpleName
    //  }
    assert(A.B.simpleName == "B$")
    assert(simpleClassName("C") == "C")
    assert(simpleClassName("a.C") == "C")
    assert(simpleClassName("a.C$") == "C$")
    assert(simpleClassName("aa.bbb.C") == "C")
    assert(simpleClassName("aa.B$C") == "C")
    assert(simpleClassName("aa.B$CC") == "CC")

  "shortClassName" in:
    assert(ScalaUtilsTest.getClass.shortClassName == "ScalaUtilsTest")
    assert((new ScalaUtilsTest.Inner).getClass.shortClassName == "ScalaUtilsTest.Inner")
    class Local
    assert((new Local).getClass.shortClassName == "ScalaUtilsTest$Local.1")
    assert(7.getClass.shortClassName == "Int")

  "Function1.withToString" in:
    def function(o: Int) = 2*o
    val f = function
    val g = f.withToString("My function")
    g(3) shouldEqual f(3)
    g.toString() shouldEqual "My function"

  "withToString" in:
    val f = function0WithToString("TEST") { 7 }
    assert(f() == 7)
    assert(f.toString == "TEST")

  "function1WithToString" in:
    val f = function1WithToString("TEST")((i: Int) => 2 * i)
    assert(f(3) == 6)
    assert(f.toString == "TEST")

  "Option" - {
    "fold_" in:
      def f(a: Option[Int]): Either[Int, String] = a.fold_(Left(0), o => Right(o.toString))
      assert(f(None) == Left(0))
      assert(f(Some(7)) == Right("7"))

    "!!" in:
      assert((false !! Problem("PROBLEM")) == Left(Problem("PROBLEM")))
      assert((true !! Problem("PROBLEM")) == Right(()))
  }

  "Throwable" - {
    "rootCause" in:
      new Exception("A", new Exception("B", new Exception("ROOT"))).rootCause.getMessage shouldEqual "ROOT"

    "toStringWithCauses" in:
      assert(new Exception("TEST").toStringWithCauses == "TEST")
      assert(new Exception("TEST", new IllegalStateException("STATE")).toStringWithCauses ==
        "TEST, caused by: IllegalStateException: STATE")

      val e = new Exception("TEST")
      e.addSuppressed(new IllegalStateException("SUPPRESSED 1", new Exception("CAUSE", new Exception("NESTED CAUSE"))))
      e.addSuppressed(new IllegalStateException("SUPPRESSED 2"))
      assert(e.toStringWithCauses ==
        "TEST " +
          "[suppressed: IllegalStateException: SUPPRESSED 1, caused by: CAUSE, caused by: NESTED CAUSE] " +
          "[suppressed: IllegalStateException: SUPPRESSED 2]")

    "toSimplifiedString" in:
      assert(new RuntimeException("ERROR").toSimplifiedString == "ERROR")
      assert(new IllegalArgumentException("ERROR").toSimplifiedString == "ERROR")
      assert(new StandardPublicException("ERROR").toSimplifiedString == "ERROR")
      assert(new IllegalStateException("ERROR").toSimplifiedString == "IllegalStateException: ERROR")

    "nullIfNoStackTrace" in:
      val withStackTrace = new Throwable
      val withoutStackTrace = new Throwable("TEST") with NoStackTrace
      assert(withStackTrace.nullIfNoStackTrace eq withStackTrace)
      assert(withoutStackTrace.nullIfNoStackTrace eq null)
      logger.debug(s"nullIfNoStackTrace: ${withoutStackTrace.toStringWithCauses}", withoutStackTrace)

    "ifStackTrace" in:
      val withStackTrace = new Throwable
      val withoutStackTrace = new Throwable("TEST") with NoStackTrace
      assert(withStackTrace.ifStackTrace.get eq withStackTrace)
      assert(withoutStackTrace.ifStackTrace == None)
  }

  "narrow" in:
    val s: Any = "Hej!"
    val string = cast[String](s)
    (string: String) shouldEqual "Hej!"
    intercept[ClassCastException]{ cast[String](123) } .getMessage shouldEqual "Expected java.lang.String but got java.lang.Integer: 123"

  "checkedCast" in:
    val s: Any = "Hej!"
    val string = checkedCast[String](s)
    string shouldEqual Right("Hej!")
    assert(checkedCast[String](123) == Left(Problem("Expected java.lang.String but got java.lang.Integer: 123")))
    assert(checkedCast[String](null).left.exists(_.throwable.isInstanceOf[NullPointerException]))

  "ifCast" in:
    val s: Any = "Hej!"
    val string = ifCast[String](s)
    assert(string == Some("Hej!"))
    assert(ifCast[String](123) == None)
    assert(ifCast[String](null) == None)

  "someUnless" in:
    someUnless(7, none = 0) shouldEqual Some(7)
    someUnless(0, none = 0) shouldEqual None

  "|> (apply function, pipeline operator)" in:
    assert((3 |> (_ * 7)) == 3 * 7)
    def double(a: Int) = 2 * a
    assert((3 |> double) == double(3))

  "pipeIf" in:
    assert(3.pipeIf(true)(_ * 7) == 3 * 7)
    assert(3.pipeIf(false)(_ * 7) == 3)

  "pipeMaybe" in:
    val none: Option[String] = None
    val some: Option[String] = Some("B")
    assert("A".pipeMaybe(none)(_ + _) == "A")
    assert("A".pipeMaybe(some)(_ + _) == "AB")

  "AtomicBoolean.switchOn" in:
    val a = new AtomicBoolean
    var x = 0
    a.switchOn { x = 1 }
    assert(x == 1)
    a.switchOn { x = 2 }
    assert(x == 1)
    a.switchOff { x = 3 }
    assert(x == 3)
    a.switchOff { x = 4 }
    assert(x == 3)

  "Map" - {
    val map = Map(1 -> "ONE")

    "Map.checked" in:
      assert(map.checked(1) == Right("ONE"))
      assert(map.checked(2) == Left(UnknownKeyProblem("Int", 2)))

    "Map.checked as curried function" in:
      val checked: Int => Checked[String] = map.checked
      assert(checked(1) == Right("ONE"))
      assert(checked(2) == Left(UnknownKeyProblem("Int", 2)))
  }

  "MapView" - {
    val a = MapView(1 -> "ONE", 2 -> "TWO")
    val b = MapView(2 -> "dwa", 3 -> "tri")

    "collectValues" in:
      assert(a.collectValues { case "ONE" => "eins" }.toMap == Map(1 -> "eins"))

    "orElseMapView" in:
      val c = a.orElseMapView(b)
      assert(c(1) == "ONE")
      assert(c(2) == "TWO")
      assert(c(3) == "tri")
      assert(c.toSeq == Seq(1 -> "ONE", 2 -> "TWO", 3 -> "tri"))
      assert(c.toMap == Map(1 -> "ONE", 2 -> "TWO", 3 -> "tri"))

    "mapIsomorphic" in:
      val morphed: MapView[String, Boolean] =
        Map(1 -> "true", 2 -> "false")
          .view
          .mapIsomorphic(_.toString, _.toBoolean)(_.toInt)

      assert(morphed.toMap == Map(
        "1" -> true,
        "2" -> false))

      assert(morphed.get("?") == None)
      assert(morphed("1") == true)
      assert(morphed("2") == false)

      assert(!morphed.contains("?"))
      assert(!morphed.contains("0"))
      assert(morphed.contains("1"))
      assert(morphed.contains("2"))

      assert(morphed.keys.toSeq == Seq("1", "2"))

      assert(morphed.keySet == Set("1", "2"))

      assert(morphed.values.toSeq == Seq(true, false))

    "+++" in:
      val a = MapView(1 -> "ONE", 2 -> "TWO")
      val b = MapView(2 -> "dwa", 3 -> "tri")
      assert((a ++ b).toSeq == Seq(1 -> "ONE", 2 -> "TWO", 2 -> "dwa", 3 -> "tri"))
      assert((a +++ b).toSeq == Seq(1 -> "ONE", 2 -> "dwa", 3 -> "tri"))
      assert((a +++ b)(1) == "ONE")
      assert((a +++ b)(2) == "dwa")
      assert((a +++ b)(3) == "tri")
      assert((a +++ b).keys.toSeq == Seq(1, 2, 3))
      assert((a +++ b).size == 3)
  }

  "View" - {
    ":+" in:
      val view: View[Int] = View(1, 2, 3) :+ 4
      assert(view.toSeq == Seq(1, 2, 3, 4))

    "+:" in:
      val view: View[Int] = 1 +: View(2, 3, 4)
      assert(view.toSeq == Seq(1, 2, 3, 4))
  }

  "mergeOrdered" - {
    "mergeOrderedSlow" - {
      "empty" in:
        assert(!List[List[Int]]().mergeOrderedSlowBy(identity).hasNext)

      "standard case" in:
        val seqs = Seq(
          Seq(1, 2, 4, 9, 10),
          Seq(3, 5, 6, 8),
          Seq(7, 8))
        assert(seqs.mergeOrderedSlowBy(identity).toSeq == Seq(1, 2, 3, 4, 5, 6, 7, 8, 8, 9, 10))

      "mergeOrderedBy" in:
        assert(Seq(Seq(1, 2, 4), Seq(3, 5)).mergeOrderedSlowBy(identity).toSeq ==
          Seq(1, 2, 3, 4, 5))
        assert(Seq(Seq(4, 2, 1), Seq(5, 3)).mergeOrderedSlowBy(-_).toSeq ==
          Seq(5, 4, 3, 2, 1))

      if sys.props.contains("test.speed") then
        "speed, simple algorithm" in:
          testSpeed(_.mergeOrderedSlowBy(identity))

      def testSpeed(mergeOrdered: Vector[Vector[Integer]] => Iterator[Integer]) =
        val n = 10_000_000
        val buffers = Vector.fill(10)(Vector.newBuilder[Integer])
        for i <- 0 until n / 2 do buffers(Random.nextInt(7)) += i
        for i <- n / 2 until n do buffers(7 + Random.nextInt(3)) += i
        val seqs = buffers.map(_.result())
        logger.info(seqs.map(_.length).map(n => s"$n×").mkString(" "))
        for _ <- 1 to 3 do
          val t = now
          val result = mergeOrdered(seqs).toVector
          val elapsed = t.elapsed
          assert(result == (0 until n))
          logger.info(Stopwatch.itemsPerSecondString(elapsed, n, "elements"))
    }

    "mergeOrderedByOptimized" - {
      "empty" in:
        assert(!List[List[Int]]().mergeOrderedOptimizedBy(identity).hasNext)

      "standard case" in:
        val seqs = Seq(
          Seq(1, 2, 4, 9, 10),
          Seq(3, 5, 6, 8),
          Seq(7, 8))
        assert(seqs.mergeOrderedOptimizedBy(identity).toSeq == Seq(1, 2, 3, 4, 5, 6, 7, 8, 8, 9, 10))

      "mergeOrderedBy" in:
        assert(Seq(Seq(1, 2, 4), Seq(3, 5)).mergeOrderedOptimizedBy(identity).toSeq ==
          Seq(1, 2, 3, 4, 5))
        assert(Seq(Seq(4, 2, 1), Seq(5, 3)).mergeOrderedOptimizedBy(-_).toSeq ==
          Seq(5, 4, 3, 2, 1))

      "Iterator behavior" in:
        val seqs = Seq(Seq(1, 2, 3))
        assert(seqs.mergeOrderedOptimizedBy(identity).toSeq == Seq(1, 2, 3))

        val iterator = seqs.mergeOrderedOptimizedBy(identity).buffered
        assert(iterator.isInstanceOf[MergeOrderedIterator[Int, Int]])
        assert(iterator.hasNext)
        assert(iterator.hasNext)
        assert(iterator.next() == 1)
        assert(iterator.head == 2)
        assert(iterator.headOption == Some(2))
        assert(iterator.next() == 2)
        assert(iterator.next() == 3)
        assert(iterator.headOption.isEmpty)
        assert(!iterator.hasNext)
        intercept[NoSuchElementException](iterator.next())

      if sys.props.contains("test.speed") then
        "speed" in:
          testSpeed(_.mergeOrderedOptimizedBy(identity))

      def testSpeed(mergeOrdered: Vector[Vector[Integer]] => Iterator[Integer]) =
        val n = 10_000_000
        val buffers = Vector.fill(10)(Vector.newBuilder[Integer])
        for i <- 0 until n / 2 do buffers(Random.nextInt(7)) += i
        for i <- n / 2 until n do buffers(7 + Random.nextInt(3)) += i
        val seqs = buffers.map(_.result())
        logger.info(seqs.map(_.length).map(n => s"$n×").mkString(" "))
        for _ <- 1 to 3 do
          val t = now
          val result = mergeOrdered(seqs).toVector
          val elapsed = t.elapsed
          assert(result == (0 until n))
          logger.info(Stopwatch.itemsPerSecondString(elapsed, n, "elements"))
    }
  }

  "PartialFunction" - {
    val pf: PartialFunction[Int, String] =
      case 1 => "ONE"

    "PartialFunction.checked" in:
      assert(pf.checked(1) == Right("ONE"))
      assert(pf.checked(2) == Left(UnknownKeyProblem("Int", 2)))

    "PartialFunction.checked as curried function" in:
      val checked: Int => Checked[String] = pf.checked
      assert(checked(1) == Right("ONE"))
      assert(checked(2) == Left(UnknownKeyProblem("Int", 2)))

    "PartialFunction.doNotContain" in:
      assert(pf.checkNoDuplicate(1) == Left(DuplicateKey("Int", 1)))
      assert(pf.checkNoDuplicate(2) == Right(()))

    "PartialFunction.toThrowableChecked" in:
      assert(pf.checked(1) == Right("ONE"))
      assert(pf.checked(2) == Left(UnknownKeyProblem("Int", 2)))

    "PartialFunction.getOrElse" in:
      assert(pf.getOrElse(1, "DEFAULT") == "ONE")
      assert(pf.getOrElse(2, "DEFAULT") == "DEFAULT")

    "PartialFunction map" in:
      case class A(string: String)
      val pf: PartialFunction[Int, A] =
        case 1 => A("one")

      assert(pf(1) == A("one"))
      assert(pf.isDefinedAt(1))
      assert(!pf.isDefinedAt(2))
      assert(pf.applyOrElse(1, (i: Int) => A(s"else $i")) == A("one"))
      assert(pf.applyOrElse(2, (i: Int) => A(s"else $i")) == A("else 2"))

      val mappedPf = pf.map(_.string)
      assert(mappedPf(1) == "one")
      assert(mappedPf.isDefinedAt(1))
      assert(!mappedPf.isDefinedAt(2))
      assert(mappedPf.applyOrElse(1, (i: Int) => s"else $i") == "one")
      assert(mappedPf.applyOrElse(2, (i: Int) => s"else $i") == "else 2")
  }

  "Iterable[Either]" - {
    "reduceLeftEither" in:
      type T = Seq[Either[String, Int]]
      assert((Nil: T).reduceLeftEither == Right(Nil))
      assert((Seq(Left("A"), Right(1)): T).reduceLeftEither == Left("A"))
      assert((Seq(Left("A"), Right(1), Left("B")): T).reduceLeftEither == Left("AB"))
      assert((Seq(Right(1), Right(2)): T).reduceLeftEither == Right(Seq(1, 2)))
      assert((Seq(Left(Seq("A")), Right(1), Left(Seq("B")))).reduceLeftEither == Left(Seq("A", "B")))
  }

  "Either" - {
    "rightAs" in:
      type O = Either[String, Int]
      assert((Left("LEFT"): O).rightAs(true) == Left("LEFT"))
      assert((Right(7): O).rightAs(true) == Right(true))
      assert((Right(7): O).rightAs(()) eq RightUnit)  // Optimized

    "orElse" in:
      assert((Left("LEFT"): Either[String, String]).orElse(Left("WINNER")) == Left("WINNER"))
      assert((Left("LEFT"): Either[String, String]).orElse(Right("WINNER")) == Right("WINNER"))
      assert((Right("RIGHT"): Either[String, String]).orElse(Left("LOOSER")) == Right("RIGHT"))
      assert((Right("RIGHT"): Either[String, String]).orElse(Right("LOOSER")) == Right("RIGHT"))

    "combineLeft" in:
      type T = Either[String, Int]
      assert((Left("A"): T).combineLeft(Left("B"): T) == Left("AB"))
      assert((Left("A"): T).combineLeft(Right(2): T) == Left("A"))
      assert((Right(1): T).combineLeft(Left("B"): T) == Left("B"))
      assert((Right(1): T).combineLeft(Right(2): T) == Right((1, 2)))

    "combineLeftOrRight" in:
      type T = Either[String, String]
      assert((Left("A"): T).combineLeftOrRight(Left("B"): T) == Left("AB"))
      assert((Left("A"): T).combineLeftOrRight(Right("R"): T) == Left("A"))
      assert((Right("A"): T).combineLeftOrRight(Left("B"): T) == Left("B"))
      assert((Right("X"): T).combineLeftOrRight(Right("Y"): T) == Right("XY"))

    "toThrowableChecked" in:
      assert(Right[Throwable, Int](7).toThrowableChecked == Right(7))
      val t = new IllegalArgumentException
      assert(Left[Throwable, Int](t).toThrowableChecked.swap.getOrElse(null).throwable eq t)

    "toMessageOnlyChecked" in:
      assert(Right[Throwable, Int](7).toMessageOnlyChecked == Right(7))
      val t = new IllegalArgumentException("EXCEPTION")
      assert(Left[Throwable, Int](t).toMessageOnlyChecked.swap.getOrElse(null).throwable ne t)
      assert(Left[Throwable, Int](t).toMessageOnlyChecked.swap.getOrElse(null).toString == "EXCEPTION")

    "orThrow for Left(Throwable)" in:
      assert(Right[Throwable, Int](7).orThrow == 7)
      val t = new IllegalArgumentException
      intercept[IllegalArgumentException] {
        (Left[Throwable, Int](t): Either[Throwable, Int]).orThrow
      } should be theSameInstanceAs (t)

    "orThrow" in:
      assert(Right[String, Int](7).orThrow == 7)
      val t = intercept[NoSuchElementException]:
        (Left[String, Int]("LEFT"): Either[String, Int]).orThrow
      assert(t.toString == "java.util.NoSuchElementException: Either.orThrow on Left(LEFT)")

    "orThrow drops own StackTraceElements" in:
      val throwable = new Exception with NoStackTrace {}
      val t = intercept[Exception]:
        (Left(throwable): Either[Throwable, Nothing]).orThrow
      assert(t.getStackTrace.head.getMethodName startsWith "f$proxy")

    "tapEach" in:
      val left: Either[String, Int] = Left("STRING")
      assert(left.tapEach(_ => fail()) == Left("STRING"))

      val right: Either[String, Int] = Right(7)
      var x = 0
      assert(right.tapEach(x = _) == Right(7))
      assert(x == 7)

    "tapLeft" in:
      val left: Either[String, Int] = Left("LEFT")
      var x = "?"
      assert(left.tapLeft(x = _) == Left("LEFT"))
      assert(x == "LEFT")

      val right: Either[String, Int] = Right(7)
      assert(right.tapLeft(_ => fail()) == Right(7))

    ".left.orThrow" in:
      intercept[RuntimeException](Right(1).left.orThrow)
      assert(Left(1).left.orThrow == 1)

    "withStackTrace" - {
      "with stacktrace provided" in:
        assert(Right[Throwable, Int](7).withStackTrace == Right[Throwable, Int](7))
        val t = new IllegalArgumentException
        assert(t.getStackTrace.nonEmpty)
        val Left(t2) = Left[Throwable, Int](t).withStackTrace: @unchecked
        assert(t2 eq t)

      "without stacktrace provided" in:
        val u = new IllegalArgumentException with NoStackTrace
        assert(u.getStackTrace.isEmpty)
        (Left[Throwable, Int](u).withStackTrace: @unchecked) match
          case Left(uu: IllegalStateException) => assert(uu.getStackTrace.nonEmpty)
    }
  }

  "Any" - {
    "narrow" in:
      trait A
      case class A1() extends A
      case class A2() extends A
      assert(((A1(): A).narrow[A]: Checked[A]) == Right(A1()))
      assert(((A1(): A).narrow[A2]: Checked[A2]).isLeft)
      assert(((A2(): A).narrow[A2]: Checked[A]) == Right(A2()))
  }

  "Boolean" - {
    "Boolean ? value" in:
      assert((true ? 7: Option[Int]) == Some(7))
      assert((false ? 7: Option[Int]) == None)

    "Boolean ?" in:
      assert(true.? == Some(true))
      assert(false.? == None)

    "Boolean.option" in:
      assert((true option 7: Option[Int]) == Some(7))
      assert((false option 7: Option[Int]) == None)

    "Boolean.thenList" in:
      assert((true thenList 7: List[Int]) == List(7))
      assert((false thenList 7: List[Int]) == Nil)

    "Boolean.thenVector" in:
      assert((true thenVector 7: Vector[Int]) == Vector(7))
      assert((false thenVector 7: Vector[Int]) == Vector())

    "Boolean.thenSet" in:
      assert((true thenSet 7: Set[Int]) == Set(7))
      assert((false thenSet 7: Set[Int]) == Set())

    "Boolean.thenIterator" in:
      assert((true thenIterator 7: Iterator[Int]).toList == List(7))
      assert((false thenIterator 7: Iterator[Int]).isEmpty)
  }

  "shortStringToInputStream" in:
    val in = shortStringToInputStream("heiß")
    assert(in.read() == 'h'.toByte)
    assert(in.read() == 'e'.toByte)
    assert(in.read() == 'i'.toByte)
    assert(in.read() == 0xC3)
    assert(in.read() == 0x9F)
    assert(in.read() == -1)

  "Char" - {
    "utf8Length" in:
      assert('\u0000'.utf8Length == 1)
      assert('A'.utf8Length == 1)
      assert('\u007f'.utf8Length == 1)
      assert('\u0080'.utf8Length == 2)
      assert('\u6771'.utf8Length == 3)
      assert('\uffff'.utf8Length == 3)
  }

  "String" - {
    //"utf8Length" in:
    //  for base <- 0 to 0x10ffff by 0x10000 do withClue(s"base=$base: "):
    //    val string = (base to base + 0xffff).map(_.toChar).mkString
    //    assert(string.estimatedUtf8Length == string.getBytes(UTF_8).length)

    "indexOfOrLength" in:
      assert("012".indexOfOrLength('0') == 0)
      assert("012".indexOfOrLength('1') == 1)
      assert("012".indexOfOrLength('?') == 3)

    "truncateWithEllipsis" in:
      assert("".truncateWithEllipsis(0) == "")
      assert("".truncateWithEllipsis(1) == "")
      assert("".truncateWithEllipsis(4) == "")
      assert("A".truncateWithEllipsis(0) == "A")
      assert("AB".truncateWithEllipsis(0) == "AB")
      assert("ABC".truncateWithEllipsis(0) == "ABC")
      assert("ABCD".truncateWithEllipsis(0) == "...")
      assert("ABCDE".truncateWithEllipsis(0) == "...")
      assert("A".truncateWithEllipsis(1) == "A")
      assert("AB".truncateWithEllipsis(1) == "AB")
      assert("ABC".truncateWithEllipsis(1) == "ABC")
      assert("ABCD".truncateWithEllipsis(1) == "...")
      assert("ABCDE".truncateWithEllipsis(1) == "...")
      assert("A".truncateWithEllipsis(2) == "A")
      assert("AB".truncateWithEllipsis(2) == "AB")
      assert("ABC".truncateWithEllipsis(2) == "ABC")
      assert("ABCD".truncateWithEllipsis(2) == "...")
      assert("ABCDE".truncateWithEllipsis(2) == "...")
      assert("A".truncateWithEllipsis(3) == "A")
      assert("AB".truncateWithEllipsis(3) == "AB")
      assert("ABC".truncateWithEllipsis(3) == "ABC")
      assert("ABCD".truncateWithEllipsis(3) == "...")
      assert("ABCDE".truncateWithEllipsis(3) == "...")
      assert("A".truncateWithEllipsis(4) == "A")
      assert("AB".truncateWithEllipsis(4) == "AB")
      assert("ABC".truncateWithEllipsis(4) == "ABC")
      assert("ABCD".truncateWithEllipsis(4) == "ABCD")
      assert("ABCDE".truncateWithEllipsis(4) == "A...")
      assert("A".truncateWithEllipsis(5) == "A")
      assert("AB".truncateWithEllipsis(5) == "AB")
      assert("ABC".truncateWithEllipsis(5) == "ABC")
      assert("ABCD".truncateWithEllipsis(5) == "ABCD")
      assert("ABCDE".truncateWithEllipsis(5) == "ABCDE")
      assert("ABCDE".truncateWithEllipsis(6) == "ABCDE")
      assert("ABCDEF".truncateWithEllipsis(6) == "ABCDEF")
      assert("ABCDEFG".truncateWithEllipsis(6) == "ABC...")
      assert("ABCDEFGHIJKLMNOPQRSTUVWXYZ".truncateWithEllipsis(6) == "ABC...")
      val expected = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx...(length 100)"
      assert(("x" * 100).truncateWithEllipsis(50, showLength = true) == expected)
      assert(expected.length == 50)

      assert("ABCDEFGH\nXYZ".truncateWithEllipsis(6, firstLineOnly = true) == "ABC...")
      assert("A\nXYZ".truncateWithEllipsis(6, firstLineOnly = true) == "A⏎...")
      assert("\nXYZ".truncateWithEllipsis(6, firstLineOnly = true) == "⏎...")

      assert("line\n\ttab\u0000\u0080".truncateWithEllipsis(99) == "line⏎⟶tab␀�")

    "replaceChar" in:
      val empty = ""
      assert(empty.replaceChar('-', '+') eq empty)
      val a = "abc"
      assert(a.replaceChar('-', '+') eq a)
      assert(("a-b--c---").replaceChar('-', '+') == "a+b++c+++")

    "dropLastWhile" in:
      assert("".dropLastWhile(_ == '/') == "")
      assert("/".dropLastWhile(_ == '/') == "")
      assert("//".dropLastWhile(_ == '/') == "")
      assert("/abc".dropLastWhile(_ == '/') == "/abc")
      assert("/abc//".dropLastWhile(_ == '/') == "/abc")

    "firstLineLength" in:
      assert("".firstLineLength == 0)
      assert("\n".firstLineLength == 1)
      assert("\n\n".firstLineLength == 1)
      assert("123\n".firstLineLength == 4)
      assert("123\r\n456".firstLineLength == 5)
      assert("123\n456".firstLineLength == 4)
      assert("123\n456\n".firstLineLength == 4)

    "firstLineLengthN" in:
      assert("".firstLineLengthN(2) == 0)
      assert("\n".firstLineLengthN(2) == 1)
      assert("\n\n".firstLineLengthN(2) == 1)
      assert("1\n".firstLineLengthN(2) == 2)
      assert("123".firstLineLengthN(2) == 2)
      assert("123\n".firstLineLengthN(2) == 2)
      assert("123\n456".firstLineLengthN(2) == 2)
      assert("123\n456\n".firstLineLengthN(2) == 2)
  }

  "StringBuilder" - {
    "fillLeft, fillRight" in:
      val sb = new StringBuilder
      sb.fillLeft(5) {}
      sb.append('|')
      sb.fillLeft(5) { sb.append(123) }
      sb.append('|')
      sb.fillLeft(5) { sb.append(123456) }
      sb.append('|')
      sb.fillRight(5) {}
      sb.append('|')
      sb.fillRight(5) { sb.append(456) }
      sb.append('|')
      sb.fillRight(5) { sb.append(456789) }
      sb.append('|')
      assert(sb.toString == "     |  123|123456|     |456  |456789|")

    "fillLeft, fillRight with big width" in:
      val n = 1000
      val sb = new StringBuilder
      sb.fillLeft(n) { sb.append('<') }
      sb.append('|')
      sb.fillRight(n) { sb.append('>') }
      sb.append('|')
      assert(sb.toString == " " * (n-1) + "<|>" + " " * (n-1) + "|")
  }

  "Boolean ?? String" in:
    assert((false ?? "STRING") == "")
    assert((true ?? "STRING") == "STRING")

  "Boolean !! Problem" in:
    assert((false !! Problem("PROBLEM")) == Left(Problem("PROBLEM")))
    assert((true !! Problem("PROBLEM")) == Right(()))

  "Array[Byte]" - {
    "indexOfByte" in:
      assert(Array.empty[Byte].indexOfByte(7) == -1)
      assert(Array[Byte](1, 2, 3).indexOfByte(7) == -1)
      assert(Array[Byte](1, 2, 3).indexOfByte(1) == 0)
      assert(Array[Byte](1, 2, 3).indexOfByte(3) == 2)
  }

  "bytesToHex" in:
    assert(bytesToHex(Array.empty[Byte]) == "")
    assert(bytesToHex(Array[Byte](0, 1, 15, 16, 255.toByte)) == "00010f10ff")
    assert(bytesToHex((0 to 255).map(_.toByte).toArray[Byte]) ==
      "000102030405060708090a0b0c0d0e0f" +
      "101112131415161718191a1b1c1d1e1f" +
      "202122232425262728292a2b2c2d2e2f" +
      "303132333435363738393a3b3c3d3e3f" +
      "404142434445464748494a4b4c4d4e4f" +
      "505152535455565758595a5b5c5d5e5f" +
      "606162636465666768696a6b6c6d6e6f" +
      "707172737475767778797a7b7c7d7e7f" +
      "808182838485868788898a8b8c8d8e8f" +
      "909192939495969798999a9b9c9d9e9f" +
      "a0a1a2a3a4a5a6a7a8a9aaabacadaeaf" +
      "b0b1b2b3b4b5b6b7b8b9babbbcbdbebf" +
      "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf" +
      "d0d1d2d3d4d5d6d7d8d9dadbdcdddedf" +
      "e0e1e2e3e4e5e6e7e8e9eaebecedeeef" +
      "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff")

  "withStringBuidler" in:
    assert(withStringBuilder(_.append("*")) == "*")
    assert(withStringBuilder()(_.append("*")) == "*")

  "Monoid" in:
    assert(List(1, 2, 3).when(true) == List(1, 2, 3))
    assert(List(1, 2, 3).when(false) == Nil)

  "chunkStrings" in:
    def f(list: List[String]) = chunkStrings(list, 7)
    assert(f(Nil) == Nil)
    assert(f(List("")) == Nil)
    assert(f(List("A")) == List("A"))
    assert(f(
      List("a", "bc", "def", "ghij", "klmnopqr", "1234567890ABCDEFHIJ")) ==
      List("abcdefg", "hijklmn", "opqr123", "4567890", "ABCDEFH", "IJ"))

  "compilable" in:
    compilable(1 / 0)


object ScalaUtilsTest:

  private val logger = Logger[this.type]
  private final class Inner
