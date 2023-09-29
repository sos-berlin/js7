package js7.base.monixutils

import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.execution.schedulers.TestScheduler
import scala.annotation.nowarn

final class SwitchTest extends OurAsyncTestSuite:
  "Initially off" in:
    val switch = Switch(false)
    switch.isOff
      .map(assert(_))
      .*>(switch.isOn)
      .map(o => assert(!o))
      .runToFuture

  "Initially on" in:
    val switch = Switch(true)
    switch.isOn
      .map(assert(_))
      .*>(switch.isOff)
      .map(o => assert(!o))
      .runToFuture

  "switchOn" in:
    val switch = Switch(false)
    switch
      .switchOn
      .map(assert(_))
      .*>(switch.switchOn)
      .map(o => assert(!o))
      .*>(switch.isOn)
      .map(assert(_))
      .runToFuture

  "switchOff" in:
    val switch = Switch(true)
    switch
      .switchOff
      .map(assert(_))
      .*>(switch.switchOff)
      .map(o => assert(!o))
      .*>(switch.isOff)
      .map(assert(_))
      .runToFuture

  "whenOff" in:
    implicit val scheduler = TestScheduler()

    val switch = Switch(false)
    val future = switch
      .whenOff
      .*>(switch.switchOn)
      .*>(Task.race(
        switch.whenOff.map(_ => fail()),
        Task.sleep(100.ms)))
      .map(_.fold(identity _: @nowarn, identity))
      .*>(switch.switchOff)
      .*>(Task.race(
        switch.whenOff,
        Task.sleep(100.ms).map(_ => fail())))
      .as(succeed)
      .runToFuture

    scheduler.tick(100.ms)
    future

  "whenOn" in:
    implicit val scheduler = TestScheduler()

    val switch = Switch(true)
    val future = switch
      .whenOn
      .*>(switch.switchOff)
      .*>(Task.race(
        switch.whenOn.map(_ => fail()),
        Task.sleep(100.ms)))
      .*>(switch.switchOn)
      .*>(Task.race(
        switch.whenOn,
        Task.sleep(100.ms).map(_ => fail())))
      .as(succeed)
      .runToFuture

    scheduler.tick(100.ms)
    future
