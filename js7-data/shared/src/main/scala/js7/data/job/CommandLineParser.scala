package js7.data.job

import js7.base.problem.Checked

object CommandLineParser
{
  def parse(source: String): Checked[CommandLineExpression] =
    CatsCommandLineParser.parse(source)
}
