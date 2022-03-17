package js7.launcher.forwindows

import cats.syntax.traverse._
import js7.base.problem.{Checked, Problem}
import scala.collection.immutable.Seq

private object WindowsCommandLineConversion
{
  // Here is how JDK does Windows quoting:
  // https://hg.openjdk.java.net/jdk8/jdk8/jdk/file/687fd7c7986d/src/windows/classes/java/lang/ProcessImpl.java

  private val ToBeQuoted = Set(' ', '"')

  def argsToCommandLine(args: Seq[String]): Checked[String] =
    (quote(args.head.replace('/', '\\')) +: args.tail.map(quote))
      .sequence
      .map(_.mkString(" "))

  private def quote(arg: String) = {
    if (arg.contains('"'))
      Left(Problem.pure("Windows command line argument must not contain a quote (\")"))
    else
      Right(
        if (arg exists ToBeQuoted)
          "\"" + arg + '"'
        else
          arg)
  }
}
