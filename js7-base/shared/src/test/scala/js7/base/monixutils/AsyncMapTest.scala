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
    asyncMap.get(1)
      .map(maybe => assert(maybe == Some("EINS")))
      .runToFuture
  }

  "get non-existing" in {
    asyncMap.get(0)
      .map(maybe => assert(maybe == None))
      .runToFuture
  }

  "checked existing" in {
    asyncMap.checked(1)
      .map(checked => assert(checked == Right("EINS")))
      .runToFuture
  }

  "checked non-existing" in {
    asyncMap.checked(0)
      .map(checked => assert(checked == Left(UnknownKeyProblem("Int", "0"))))
      .runToFuture
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
          .tapEval(_ =>
            // assert that the lock is released
            asyncMap.get(0)
              .map(checked => assert(checked == Some("FIRST-UPDATED"))))
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
      .map(o => assert(o == Right(())))
      .runToFuture
  }

  "insert duplicate" in {
    asyncMap.insert(3, "DUPLICATE")
      .map(o => assert(o == Left(DuplicateKey("int", "3"))))
      .runToFuture
  }

  private def updateChecked(maybe: Option[String]): Task[Checked[String]] =
    maybe match {
      case None => Task(Right("FIRST"))
      case Some(_) => Task(Left(Problem("EXISTING")))
    }

  "updateChecked" in {
    asyncMap.updateChecked(4, updateChecked)
      .map(o => assert(o == Right("FIRST")))
      .flatMap(_ =>
        asyncMap.updateChecked(4, updateChecked))
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
          .tapEval(_ =>
            // assert that the lock is released
            asyncMap.get(0)
              .map(checked => assert(checked == Some("FIRST-UPDATED"))))
          .runToFuture
      }
    }
  }

  "all" in {
    asyncMap.all.map(all =>
      assert(all == Map(
        0 -> "FIRST-UPDATED",
        1 -> "EINS",
        2 -> "FIRST",
        3 -> "INSERTED",
        4 -> "FIRST")))
      .runToFuture
  }

  "size" in {
    asyncMap.size
      .map(size => assert(size == 5))
      .runToFuture
  }

  "isEmpty" in {
    asyncMap.isEmpty
      .map(isEmpty => assert(!isEmpty))
      .runToFuture
  }

  "removeAll" in {
    asyncMap.removeAll
      .map(all => assert(all == Map(
        0 -> "FIRST-UPDATED",
        1 -> "EINS",
        2 -> "FIRST",
        3 -> "INSERTED",
        4 -> "FIRST")))
      .flatMap(_ => asyncMap.all)
      .map(all => assert(all.isEmpty))
      .runToFuture
  }
}
