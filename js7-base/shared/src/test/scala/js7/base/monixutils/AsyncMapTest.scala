package js7.base.monixutils

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.catsutils.CatsExtensions.tryIt
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurAsyncTestSuite
import scala.collection.View
import scala.util.{Failure, Success}

final class AsyncMapTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  private val asyncMap = AsyncMap(Map(1 -> "EINS"))

  private def update(maybe: Option[String]): IO[String] =
    maybe.fold(IO("FIRST"))(o => IO.pure(o + "-UPDATED"))

  private def propertlyCrashingUpdate(maybe: Option[String]): IO[String] =
    IO.raiseError(new RuntimeException("CRASH"))

  private def earlyCrashingUpdate(maybe: Option[String]): IO[String] =
    throw new RuntimeException("CRASH")

  "get existing" in:
    assert(asyncMap.get(1) == Some("EINS"))

  "get non-existing" in:
    assert(asyncMap.get(0) == None)

  "checked existing" in:
    assert(asyncMap.checked(1) == Right("EINS"))

  "checked non-existing" in:
    assert(asyncMap.checked(0) == Left(UnknownKeyProblem("Int", "0")))

  "remove" in:
    asyncMap
      .update(2):
        case None => IO("ZWEI")
        case o => fail(s"UNEXPECTED: $o")
      .*>(asyncMap.remove(2))
      .flatMap(maybe => IO(assert(maybe == Some("ZWEI"))))
      .*>(asyncMap.remove(2))
      .flatMap(maybe => IO(assert(maybe == None)))

  "getAndUpdate" in:
    asyncMap.getAndUpdate(0, update)
      .map(pair => assert(pair == (None, "FIRST")))
      .*>(asyncMap.getAndUpdate(0, update))
      .map(pair => assert(pair == (Some("FIRST"), "FIRST-UPDATED")))

  "getAndUpdate, crashing" - {
    for ((upd, name) <- View(
      propertlyCrashingUpdate -> "properly",
      earlyCrashingUpdate -> "early"))
      name in:
        asyncMap.getAndUpdate(0, upd)
          .attempt
          .map:
            case Left(t) => assert(t.getMessage == "CRASH")
            case Right(_) => fail()
          .*>(IO {
            // assert that the lock is released
            assert(asyncMap.get(0) == Some("FIRST-UPDATED"))
          })
  }

  "update" in:
    asyncMap.update(2)(update)
      .map(o => assert(o == "FIRST"))

  "insert" in:
    asyncMap.insert(3, "INSERTED")
      .map(o => assert(o == Right("INSERTED")))

  "insert duplicate" in:
    asyncMap.insert(3, "DUPLICATE")
      .map(o => assert(o == Left(DuplicateKey("Int", "3"))))

  "updateChecked" - {
    def updateChecked(maybe: Option[String]): IO[Checked[String]] =
      maybe match
        case None => IO(Right("FIRST"))
        case Some(_) => IO(Left(Problem("EXISTING")))

    "standard" in:
      asyncMap.updateChecked(4)(updateChecked)
        .map(o => assert(o == Right("FIRST")))

    "update again" in:
      asyncMap.updateChecked(4)(updateChecked)
        .map(o => assert(o == Left(Problem("EXISTING"))))

    "updateChecked, crashing" - {
      def properlyCrashingUpdateChecked(maybe: Option[String]): IO[Checked[String]] =
        IO.raiseError(new RuntimeException("CRASH"))

      def earlyCrashingUpdateChecked(maybe: Option[String]): IO[Checked[String]] =
        throw new RuntimeException("CRASH")

      for ((upd, name) <- View(
        properlyCrashingUpdateChecked -> "properly",
        earlyCrashingUpdateChecked -> "early"))
        name in:
          asyncMap.updateChecked(0)(upd)
            .tryIt
            .map:
              case Success(_) => fail()
              case Failure(t) => assert(t.getMessage == "CRASH")
            .flatTap(_ => IO:
              // assert that the lock is released
              assert(asyncMap.get(0) == Some("FIRST-UPDATED")))
    }
  }

  "updateCheckedWithResult" - {
    val asyncMap = AsyncMap(Map(0 -> "ZERO"))

    def updateCheckedWithResult(maybe: Option[String]): IO[Checked[(String, Int)]] =
      maybe match
        case None => IO(Right("FIRST" -> 7))
        case Some(_) => IO(Left(Problem("EXISTING")))

    "standard, check result is 7" in:
      asyncMap.updateCheckedWithResult(4, updateCheckedWithResult)
        .map(o => assert(o == Right(7)))
        .*>(asyncMap.updateCheckedWithResult(4, updateCheckedWithResult))
        .map(o => assert(o == Left(Problem("EXISTING"))))

    "standard, check updated value is FIRST" in:
      assert(asyncMap.get(4) == Some("FIRST"))

    "update again" in:
      asyncMap.updateCheckedWithResult(4, updateCheckedWithResult)
        .map(o => assert(o == Left(Problem("EXISTING"))))

    "updateCheckedWithResult, crashing" - {
      def properlyCrashingUpdateChecked(maybe: Option[String]): IO[Checked[(String, Int)]] =
        IO.raiseError(new RuntimeException("CRASH"))

      def earlyCrashingUpdateChecked(maybe: Option[String]): IO[Checked[(String, Int)]] =
        throw new RuntimeException("CRASH")

      for ((upd, name) <- View(
        properlyCrashingUpdateChecked -> "properly",
        earlyCrashingUpdateChecked -> "early"))
        name in:
          asyncMap.updateCheckedWithResult(0, upd)
            .tryIt
            .map:
              case Success(_) => fail()
              case Failure(t) => assert(t.getMessage == "CRASH")
            .flatTap(_ => IO:
              // assert that the lock is released
              assert(asyncMap.get(0) == Some("ZERO")))

    //???
    }
  }

  "unsafeToMap" in:
    assert(asyncMap.unsafeToMap == Map(
      0 -> "FIRST-UPDATED",
      1 -> "EINS",
      2 -> "FIRST",
      3 -> "INSERTED",
      4 -> "FIRST"))

  "size" in:
    assert(asyncMap.size == 5)

  "isEmpty" in:
    assert(!asyncMap.isEmpty)

  "removeConditional" in:
    asyncMap.removeConditional(_._1 <= 2)
      .map(all => assert(all == Map(
        0 -> "FIRST-UPDATED",
        1 -> "EINS",
        2 -> "FIRST")))
      .map(_ => assert(asyncMap.unsafeToMap == Map(
        3 -> "INSERTED",
        4 -> "FIRST")))

  "removeAll" in:
    asyncMap.removeAll
      .map(all => assert(all == Map(
        3 -> "INSERTED",
        4 -> "FIRST")))
      .map(_ => assert(asyncMap.unsafeToMap.isEmpty))

  "stop" - {
    "empty" - {
      "stop" in:
        val asyncMap = AsyncMap.stoppable[Int, String]()
        (for
          _ <- asyncMap.initiateStop
          checked <- asyncMap.insert(1, "NOT ALLOWED")
          _ = assert(checked == Left(Problem("AsyncMap[Int, String] is being stopped")))
        yield succeed
        ).unsafeToFuture()

      "initiateStopWithProblem" in:
        val asyncMap = AsyncMap.stoppable[Int, String]()
        val myProblem = Problem("MY PROBLEM")
        (for
          _ <- asyncMap.initiateStopWithProblem(myProblem)
          checked <- asyncMap.insert(1, "NOT ALLOWED")
          _ = assert(checked == Left(myProblem))
          _ = assert(asyncMap.isStoppingWith(myProblem))
          _ = assert(!asyncMap.isStoppingWith(Problem("OTHER")))
        yield succeed
        ).unsafeToFuture()

      "initiateStopWithProblemIfEmpty" - {
        "non empty" in:
          val asyncMap = AsyncMap.stoppable[Int, String]()
          val myProblem = Problem("MY PROBLEM")
          (for
            _ <- asyncMap.insert(1, "SOMETHING")
            ok <- asyncMap.initiateStopWithProblemIfEmpty(myProblem)
            _ = assert(!ok)
            checked <- asyncMap.insert(2, "NOT ALLOWED")
            _ = assert(checked.isRight)
            _ = assert(!asyncMap.isStoppingWith(myProblem))
          yield succeed
          ).unsafeToFuture()

        "empty" in:
          val asyncMap = AsyncMap.stoppable[Int, String]()
          val myProblem = Problem("MY PROBLEM")
          (for
            ok <- asyncMap.initiateStopWithProblemIfEmpty(myProblem)
            _ = assert(ok)
            checked <- asyncMap.insert(1, "NOT ALLOWED")
            _ = assert(checked == Left(myProblem))
            _ = assert(asyncMap.isStoppingWith(myProblem))
          yield succeed
          ).unsafeToFuture()
      }
    }

    lazy val asyncMap = AsyncMap.stoppable[Int, String]()

    "non empty" - {
      lazy val fillAsyncMap =
        asyncMap.insert(1, "EINS")
          .as(succeed).unsafeToFuture()


      "update is allowed" in:
        fillAsyncMap

        (for
          checked <- asyncMap.insert(1, "EINS")
          _ = IO(assert(checked.isRight))
          _ <- asyncMap.initiateStop

          _ <- asyncMap.getAndUpdate(1, {
            case Some("EINS") => IO("EINS*")
            case _ => fail()
          })
          _ <- IO(assert(asyncMap.get(1) == Some("EINS*")))

          tried <- asyncMap.getOrElseUpdate(2, IO("ZWEI")).tryIt
          _ <- IO(assert(tried.isFailure))

          _ <- asyncMap.remove(1)
          _ <- asyncMap.whenStopped
        yield succeed
        ).unsafeToFuture()
    }
  }
