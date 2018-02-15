package com.sos.jobscheduler.core.filebased

import cats.data.Validated.Invalid
import cats.syntax.flatMap._
import com.sos.jobscheduler.base.problem.Checked.monad
import com.sos.jobscheduler.base.problem.Checked.ops.RichOption
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.data.filebased.{SourceType, TypedPath}
import java.nio.file.Path
import scala.collection.immutable.Iterable

/**
  * @author Joacim Zschimmer
  */
object TypedPaths {

  def fileToTypedPath(companions: Iterable[TypedPath.AnyCompanion], path: Path, stripDirectory: Path): Checked[(TypedPath, SourceType)] = {
    val normalizedPath = toSlashes(path)
    var normalizedDir = toSlashes(stripDirectory)
    if (!normalizedDir.endsWith("/")) normalizedDir += "/"
    if (!normalizedPath.startsWith(normalizedDir))
      Invalid(Problem(s"Path '$normalizedPath' does not start with '$normalizedDir'"))
    else
      relativeFilePathToTypedPath(companions, file = normalizedPath drop normalizedDir.length - 1)
  }

  private def relativeFilePathToTypedPath(companions: Iterable[TypedPath.AnyCompanion], file: String): Checked[(TypedPath, SourceType)] =
    (for (companion ← companions.iterator) yield
      for (pathAndSourceType ← companion.fromFile(file))
        yield pathAndSourceType)
    .find(_.isValid)
    .toChecked(Problem(s"File '${file.stripPrefix("/")}' is not recognized as a configuration file"))
    .flatten

  private def toSlashes(path: Path): String = {
    var result = path.toString
    if (result contains path.getFileSystem.getSeparator) {
      result = result.replaceAllLiterally(path.getFileSystem.getSeparator, "/")
    }
    result
  }
}
