package js7.data.job

import java.nio.file.{Path, Paths}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat

final case class CommandLine(arguments: Seq[String]):
  assertThat(arguments.nonEmpty)

  def file: Path = Paths.get(arguments.head)

  override def toString = arguments
    .map { arg =>
      if arg.contains(' ') || arg.contains('\'') then
        "'" + arg.replace("'", "\''") + "'"
      else
        arg
    }
    .mkString(" ")


object CommandLine:
  def fromFile(file: Path) = new CommandLine(Seq(file.toString))

  def checked(arguments: Seq[String]): Checked[CommandLine] =
    if arguments.isEmpty then Left(Problem.pure("Command line must not be empty"))
    else Right(new CommandLine(arguments))
