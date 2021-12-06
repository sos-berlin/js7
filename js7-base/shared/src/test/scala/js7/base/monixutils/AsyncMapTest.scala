package js7.base.monixutils

import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec
import scala.collection.View
import scala.util.{Failure, Success}

final class AsyncMapTest extends AsyncFreeSpec
{
  private val asyncMap = AsyncMap(Map(1 -> "EINS"))

  private def update(maybe: Option[String]): Task[String] =
    maybe.fold(Task("FIRST"))(o => Task.pure(o + "-UPDATED"))

  private def propertlyCrashingUpdate(maybe: Option[String]): Task[String] =
    Task.raiseError(new RuntimeException("CRASH"))

  private def earlyCrashingUpdate(maybe: Option[String]): Task[String] =
    throw new RuntimeException("CRASH")

  "get existing" in {
    assert(asyncMap.get(1) == Some("EINS"))
  }

  "get non-existing" in {
    assert(asyncMap.get(0) == None)
  }

  "checked existing" in {
    assert(asyncMap.checked(1) == Right("EINS"))
  }

  "checked non-existing" in {
    assert(asyncMap.checked(0) == Left(UnknownKeyProblem("Int", "0")))
  }

  "remove" in {
    asyncMap
      .update(2, {
        case None => Task("ZWEI")
        case o => fail(s"UNEXPECTED: $o")
      })
      .flatMap(_ => asyncMap.remove(2))
      .flatMap(maybe => Task(assert(maybe == Some("ZWEI"))))
      .flatMap(_ => asyncMap.remove(2))
      .flatMap(maybe => Task(assert(maybe == None)))
      .runToFuture
  }

  "getAndUpdate" in {
    asyncMap.getAndUpdate(0, update)
      .map(pair => assert(pair == (None, "FIRST")))
      .flatMap(_ =>
        asyncMap.getAndUpdate(0, update))
      .map(pair => assert(pair == (Some("FIRST"), "FIRST-UPDATED")))
      .runToFuture
  }

  "getAndUpdate, crashing" - {
    for ((upd, name) <- View(
      propertlyCrashingUpdate _ -> "properly",
      earlyCrashingUpdate _ -> "early"))
    {
      name in {
        asyncMap.getAndUpdate(0, upd)
          .materialize
          .map {
            case Success(_) => fail()
            case Failure(t) => assert(t.getMessage == "CRASH")
          }
          .tapEval(_ => Task {
            // assert that the lock is released
            assert(asyncMap.get(0) == Some("FIRST-UPDATED"))
          })
          .runToFuture
      }
    }
  }

  "update" in {
    asyncMap.update(2, update)
      .map(o => assert(o == "FIRST"))
      .runToFuture
  }

  "insert" in {
    asyncMap.insert(3, "INSERTED")
      .map(o => assert(o == Right("INSERTED")))
      .runToFuture
  }

  "insert duplicate" in {
    asyncMap.insert(3, "DUPLICATE")
      .map(o => assert(o == Left(DuplicateKey("int", "3"))))
      .runToFuture
  }

  "updateChecked" - {
     def updateChecked(maybe: Option[String]): Task[Checked[String]] =
      maybe match {
        case None => Task(Right("FIRST"))
        case Some(_) => Task(Left(Problem("EXISTING")))
      }

    "standard" in {
      asyncMap.updateChecked(4, updateChecked)
        .map(o => assert(o == Right("FIRST")))
        .runToFuture
    }

    "update again" in {
      asyncMap.updateChecked(4, updateChecked)
        .map(o => assert(o == Left(Problem("EXISTING"))))
        .runToFuture
    }

    "updateChecked, crashing" - {
      def properlyCrashingUpdateChecked(maybe: Option[String]): Task[Checked[String]] =
        Task.raiseError(new RuntimeException("CRASH"))

      def earlyCrashingUpdateChecked(maybe: Option[String]): Task[Checked[String]] =
        throw new RuntimeException("CRASH")

      for ((upd, name) <- View(
        properlyCrashingUpdateChecked _ -> "properly",
        earlyCrashingUpdateChecked _ -> "early"))
      {
        name in {
          asyncMap.updateChecked(0, upd)
            .materialize
            .map {
              case Success(_) => fail()
              case Failure(t) => assert(t.getMessage == "CRASH")
            }
            .tapEval(_ => Task {
              // assert that the lock is released
              assert(asyncMap.get(0) == Some("FIRST-UPDATED"))
            })
            .runToFuture
        }
      }
    }
  }

  "updateCheckedWithResult" - {
    val asyncMap = AsyncMap(Map(0 -> "ZERO"))

    def updateCheckedWithResult(maybe: Option[String]): Task[Checked[(String, Int)]] =
      maybe match {
        case None => Task(Right("FIRST" -> 7))
        case Some(_) => Task(Left(Problem("EXISTING")))
      }

    "standard, check result is 7" in {
      asyncMap.updateCheckedWithResult(4, updateCheckedWithResult)
        .map(o => assert(o == Right(7)))
        .flatMap(_ =>
          asyncMap.updateCheckedWithResult(4, updateCheckedWithResult))
        .map(o => assert(o == Left(Problem("EXISTING"))))
        .runToFuture
    }

    "standard, check updated value is FIRST" in {
      assert(asyncMap.get(4) == Some("FIRST"))
    }

    "update again" in {
      asyncMap.updateCheckedWithResult(4, updateCheckedWithResult)
        .map(o => assert(o == Left(Problem("EXISTING"))))
        .runToFuture
    }

    "updateCheckedWithResult, crashing" - {
      def properlyCrashingUpdateChecked(maybe: Option[String]): Task[Checked[(String, Int)]] =
        Task.raiseError(new RuntimeException("CRASH"))

      def earlyCrashingUpdateChecked(maybe: Option[String]): Task[Checked[(String, Int)]] =
        throw new RuntimeException("CRASH")

      for ((upd, name) <- View(
        properlyCrashingUpdateChecked _ -> "properly",
        earlyCrashingUpdateChecked _ -> "early"))
      {
        name in {
          asyncMap.updateCheckedWithResult(0, upd)
            .materialize
            .map {
              case Success(_) => fail()
              case Failure(t) => assert(t.getMessage == "CRASH")
            }
            .tapEval(_ => Task {
              // assert that the lock is released
              assert(asyncMap.get(0) == Some("ZERO"))
            })
            .runToFuture
        }
      }
    }

    //???
  }

  "toMap" in {
    assert(asyncMap.toMap == Map(
      0 -> "FIRST-UPDATED",
      1 -> "EINS",
      2 -> "FIRST",
      3 -> "INSERTED",
      4 -> "FIRST"))
  }

  "size" in {
    assert(asyncMap.size == 5)
  }

  "isEmpty" in {
    assert(!asyncMap.isEmpty)
  }

  "removeAll" in {
    asyncMap.removeAll
      .map(all => assert(all == Map(
        0 -> "FIRST-UPDATED",
        1 -> "EINS",
        2 -> "FIRST",
        3 -> "INSERTED",
        4 -> "FIRST")))
      .map(_ => assert(asyncMap.toMap.isEmpty))
      .runToFuture
  }
}
