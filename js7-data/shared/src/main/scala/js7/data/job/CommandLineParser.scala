package js7.data.job

import js7.base.problem.Checked
import js7.data.parser.UseFastparse

object CommandLineParser
{
  def parse(source: String): Checked[CommandLineExpression] =
    if (UseFastparse)
      FastparseCommandLineParser.parse(source)
    else
      CatsCommandLineParser.parse(source)
}
