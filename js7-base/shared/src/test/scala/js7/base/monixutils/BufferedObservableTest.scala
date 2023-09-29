package js7.base.monixutils

import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import monix.execution.Scheduler.Implicits.traced
import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable

final class BufferedObservableTest extends OurAsyncTestSuite
{
  private val delay = 1.s
  private val shortDelay = 1.s - 100.ms
  private val longDelay = 1.s + 100.ms
  private val testScheduler = TestScheduler()

  "timespan" in {
    val obs = Observable("A") ++
      Observable("B-1", "B-2").delayExecution(longDelay) ++
      Observable("B-3", "B-4").delayExecution(shortDelay) ++
      Observable("C").delayExecution(longDelay) ++
      Observable("D-1", "D-2").delayExecution(longDelay) ++
      Observable("D-3").delayExecution(shortDelay)
    val whenList = obs.buffer(Some(delay), Int.MaxValue).toListL.runToFuture(testScheduler)
    testScheduler.tick(100.s)

    for list <- whenList yield {
      assert(list == List(
        List("A"),
        List("B-1", "B-2", "B-3", "B-4"),
        List("C"),
        List("D-1", "D-2", "D-3")))
    }
  }

  "timespan and maxCount" in {
    val obs =
      Observable(".........1.........2", ".........3.........4")
        .delayExecution(longDelay) ++
      Observable(".........5.........6", ".........7.........8")
        .delayExecution(shortDelay) ++
      Observable(".........9........10")
    val whenList = obs.buffer(Some(delay), maxCount = 50, toWeight = _.length).toListL.runToFuture(testScheduler)
    testScheduler.tick(100.s)

    for list <- whenList yield {
      assert(list == List(
        List(".........1.........2", ".........3.........4"),
        List(".........5.........6", ".........7.........8"),
        List(".........9........10")))
    }
  }
}
