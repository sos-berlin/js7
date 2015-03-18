package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.common.commandline.CommandLineArguments
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments

/**
 * @author Joacim Zschimmer
 */
object Main {
  def main(args: Array[String]): Unit = {
    try {
      val arguments = Arguments(args)
      val startArguments = new TaskStartArguments(controllerAddress = arguments.controllerAddress)
      SimpleTaskServer.run(startArguments)
    } catch {
      case t: Throwable ⇒
        System.err.println(t.toString)
        t.printStackTrace(System.err)
        System.exit(1)
    }
  }

  private case class Arguments(controllerAddress: String)

  private object Arguments {
    def apply(args: Seq[String]) = {
      val a = CommandLineArguments(args)
      val controllerAddress = a.string("-controller=")
      a.requireNoMoreArguments()
      new Arguments(controllerAddress = controllerAddress)
    }
  }
}
