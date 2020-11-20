package js7.data.job

import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat

final case class CommandLine(arguments: Seq[String]) {
  assertThat(arguments.nonEmpty)
}

object CommandLine
{
  def checked(arguments: Seq[String]): Checked[CommandLine] =
    if (arguments.isEmpty) Left(Problem.pure("Command line must not be empty"))
    else Right(new CommandLine(arguments))
}
