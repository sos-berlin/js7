package js7.base.monixutils

import cats.effect.IO
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
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
      .map(assert(_))
      .*>(switch.switchOn)
      .map(o => assert(!o))
      .*>(switch.isOn)
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
    val switch = Switch(false)
    switch
      .whenOff
      .*>(switch.switchOn)
      .*>(IO.race(
        switch.whenOff.map(_ => fail()),
        IO.sleep(200.ms)))
      .map(_.fold(identity _: @nowarn, identity))
      .*>(switch.switchOff)
      .*>(IO.race(
        switch.whenOff,
        IO.sleep(200.ms).map(_ => fail())))
      .as(succeed)

  "whenOn" in:
    val switch = Switch(true)
    switch
      .whenOn
      .*>(switch.switchOff)
      .*>(IO.race(
        switch.whenOn.map(_ => fail()),
        IO.sleep(200.ms)))
      .*>(switch.switchOn)
      .*>(IO.race(
        switch.whenOn,
        IO.sleep(200.ms).map(_ => fail())))
      .as(succeed)
