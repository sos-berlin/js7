package js7.data_for_java.controller

import js7.data.controller.ControllerCommand.ShutDown
import org.scalatest.freespec.AnyFreeSpec

final class JControllerCommandTest extends AnyFreeSpec
{
  "fromJson" in {
    val command = JControllerCommand
      .fromJson("""{ "TYPE": "ShutDown" }""")
      .getOrElseThrow(_.throwable)
    assert(command == JControllerCommand(ShutDown()))
  }
}
