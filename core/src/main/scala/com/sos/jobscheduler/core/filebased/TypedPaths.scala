package com.sos.jobscheduler.core.filebased

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.data.filebased.{SourceType, TypedPath}
import java.nio.file.Path
import scala.collection.immutable.Iterable

/**
  * @author Joacim Zschimmer
  */
object TypedPaths {

  def fileToTypedPath(companions: Iterable[TypedPath.AnyCompanion], file: Path): Checked[(TypedPath, SourceType)] = {
    val string = TypedPath.fileToString(file)
    companions.iterator
      .map(_.fromFile(string))
      .collectFirst { case Some(o) ⇒
        o flatMap { case (typedPath, sourceType) ⇒ typedPath.officialSyntaxChecked map (_ → sourceType) }
      }
      .toChecked(UnrecognizedFileProblem(file))
      .flatten
  }

  final case class UnrecognizedFileProblem(file: Path)
  extends Problem.Lazy(s"File '${file.toString.stripPrefix("/")}' is not recognized as a configuration file")
}
