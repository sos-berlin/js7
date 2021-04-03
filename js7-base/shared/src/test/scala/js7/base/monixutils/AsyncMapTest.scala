package js7.base.monixutils

import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.problem.{Checked, Problem}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec

final class AsyncMapTest extends AsyncFreeSpec
{
  private val asyncMap = AsyncMap(Map(1 -> "EINS"))

  private def update(maybe: Option[String]): Task[String] =
    maybe.fold(Task("FIRST"))(o => Task.pure(o + "-UPDATED"))

  "get existing" in {
    asyncMap.get(1)
      .map(checked => assert(checked == Right("EINS")))
      .runToFuture
  }

  "get non-existing" in {
    asyncMap.get(0)
      .map(checked => assert(checked == Left(UnknownKeyProblem("Int", "0"))))
      .runToFuture
  }

  "remove" in {
    asyncMap.update(2, { case None => Task("ZWEI") })
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

  "update" in {
    asyncMap.update(2, update)
      .map(o => assert(o == "FIRST"))
      .runToFuture
  }

  "updateChecked" in {
    def updateChecked(maybe: Option[String]): Task[Checked[String]] =
      maybe match {
        case None => Task(Right("FIRST"))
        case Some(_) => Task(Left(Problem("EXISTING")))
      }

    asyncMap.updateChecked(3, updateChecked)
      .map(o => assert(o == Right("FIRST")))
      .flatMap(_ =>
        asyncMap.updateChecked(3, updateChecked))
      .map(o => assert(o == Left(Problem("EXISTING"))))
      .runToFuture
  }

  "all" in {
    asyncMap.all.map(all =>
      assert(all == Map(
        0 -> "FIRST-UPDATED",
        1 -> "EINS",
        2 -> "FIRST",
        3 -> "FIRST")))
      .runToFuture
  }

  "removeAll" in {
    asyncMap.removeAll
      .map(all => assert(all == Map(
        0 -> "FIRST-UPDATED",
        1 -> "EINS",
        2 -> "FIRST",
        3 -> "FIRST")))
      .flatMap(_ => asyncMap.all)
      .map(all => assert(all.isEmpty))
      .runToFuture
  }
}
