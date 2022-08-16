package js7.data_for_java.controller

import js7.base.test.Test
import js7.data.controller.ControllerCommand.ShutDown

final class JControllerCommandTest extends Test
{
  "fromJson" in {
    val command = JControllerCommand
      .fromJson("""{ "TYPE": "ShutDown" }""")
      .getOrElseThrow(_.throwable)
    assert(command == JControllerCommand(ShutDown()))
  }
}
