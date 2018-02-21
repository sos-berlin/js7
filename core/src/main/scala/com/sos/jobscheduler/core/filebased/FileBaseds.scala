package com.sos.jobscheduler.core.filebased

import cats.data.Validated.{Invalid, Valid}
import cats.instances.vector._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.problem.Checked.monad
import com.sos.jobscheduler.base.problem.Checked.ops._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.core.filebased.FileBasedReader.readDirectoryTreeFlattenProblems
import com.sos.jobscheduler.data.filebased.FileBasedEvent.FileBasedAdded
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedEvent, TypedPath}
import java.nio.file.Path
import scala.collection.immutable.{Iterable, Seq}

/**
  * @author Joacim Zschimmer
  */
object FileBaseds
{
  def readDirectory(directory: Path, readers: Iterable[FileBasedReader], existingFileBaseds: Iterable[FileBased]): Checked[Seq[FileBasedEvent]] = {
    val pathToFileBased = existingFileBaseds toKeyedMap (_.path)
    val checkedFileBaseds = readDirectoryTreeFlattenProblems(directory, readers)
    for {
      fileBaseds ← checkedFileBaseds
      fileBased ← fileBaseds.toVector traverse merge(pathToFileBased) map (_.flatten)
    } yield fileBased
  }

  private def merge(pathToFileBased: Map[TypedPath, FileBased])(fileBased: FileBased): Checked[Option[FileBasedEvent]] =
    pathToFileBased.get(fileBased.path) match {
      case Some(existing) if existing == fileBased ⇒
        Valid(None)

      case Some(_) ⇒
        Invalid(Problem(s"Duplicate: ${fileBased.path} (change is not yet implemented)"))

      case None ⇒
        Valid(Some(FileBasedAdded(fileBased)))
    }
}
