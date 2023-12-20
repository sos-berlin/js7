package js7.base.monixutils

import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import cats.effect.IO
import js7.base.catsutils.CatsEffectExtensions.adHocInfo
import js7.base.log.Logger
import js7.base.time.TestScheduler
import scala.annotation.nowarn

final class SwitchTest extends OurAsyncTestSuite:

  "Initially off" in:
    val switch = Switch(false)
    switch.isOff
      .map(assert(_))
      .*>(switch.isOn)
      .map(o => assert(!o))

  "Initially on" in:
    val switch = Switch(true)
    switch.isOn
      .map(assert(_))
      .*>(switch.isOff)
      .map(o => assert(!o))

  "switchOn" in:
    val switch = Switch(false)
    switch
      .switchOn
      .adHocInfo(o => s"### o=$o")
      .map(assert(_))
      .adHocInfo(s"### switchOn")
      .*>(switch.switchOn)
      .adHocInfo(o => s"### o=$o")
      .map(o => assert(!o))
      .*>(switch.isOn)
      .adHocInfo(o => s"### isOn => $o")
      .map(assert(_))

  "switchOff" in:
    val switch = Switch(true)
    switch
      .switchOff
      .map(assert(_))
      .*>(switch.switchOff)
      .map(o => assert(!o))
      .*>(switch.isOff)
      .map(assert(_))

  "whenOff" in:
    implicit val scheduler = TestScheduler()

    val switch = Switch(false)
    val future = switch
      .whenOff
      .*>(switch.switchOn)
      .*>(IO.race(
        switch.whenOff.map(_ => fail()),
        IO.sleep(100.ms)))
      .map(_.fold(identity _: @nowarn, identity))
      .*>(switch.switchOff)
      .*>(IO.race(
        switch.whenOff,
        IO.sleep(100.ms).map(_ => fail())))
      .as(succeed)
      .unsafeToFuture()

    scheduler.tick(100.ms)
    future

  "whenOn" in:
    implicit val scheduler = TestScheduler()

    val switch = Switch(true)
    val future = switch
      .whenOn
      .*>(switch.switchOff)
      .*>(IO.race(
        switch.whenOn.map(_ => fail()),
        IO.sleep(100.ms)))
      .*>(switch.switchOn)
      .*>(IO.race(
        switch.whenOn,
        IO.sleep(100.ms).map(_ => fail())))
      .as(succeed)
      .unsafeToFuture()

    scheduler.tick(100.ms)
    future
