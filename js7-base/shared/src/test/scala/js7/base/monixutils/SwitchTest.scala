package js7.base.monixutils

import js7.base.time.ScalaTime._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec

final class SwitchTest extends AsyncFreeSpec
{
  "Initially off" in {
    val switch = Switch(false)
    switch.isOff
      .map(assert(_))
      .*>(switch.isOn)
      .map(o => assert(!o))
      .runToFuture
  }

  "Initially on" in {
    val switch = Switch(true)
    switch.isOn
      .map(assert(_))
      .*>(switch.isOff)
      .map(o => assert(!o))
      .runToFuture
  }

  "switchOn" in {
    val switch = Switch(false)
    switch
      .switchOn
      .map(assert(_))
      .*>(switch.switchOn)
      .map(o => assert(!o))
      .*>(switch.isOn)
      .map(assert(_))
      .runToFuture
  }

  "switchOff" in {
    val switch = Switch(true)
    switch
      .switchOff
      .map(assert(_))
      .*>(switch.switchOff)
      .map(o => assert(!o))
      .*>(switch.isOff)
      .map(assert(_))
      .runToFuture
  }

  "whenOff" in {
    val switch = Switch(false)
    switch
      .whenOff
      .*>(switch.switchOn)
      .*>(Task.race(
        switch.whenOff.as(false),
        Task.sleep(100.ms).as(true)))
      .map(_.fold(identity, identity))
      .map(assert(_))
      .runToFuture
  }
}