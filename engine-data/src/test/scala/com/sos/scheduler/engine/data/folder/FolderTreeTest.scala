package com.sos.scheduler.engine.data.folder

import com.sos.scheduler.engine.data.folder.FolderTree.Leaf
import com.sos.scheduler.engine.data.folder.FolderTreeTest._
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class FolderTreeTest extends FreeSpec {

  "FolderTree.split" in {
    assert(FolderTree.split(JobPath("/a")) == Vector("a"))
    assert(FolderTree.split(JobPath("/a/b")) == Vector("a", "b"))
    assert(FolderTree.split(ProcessClassPath.Default) == Vector(""))
  }

  "FolderTree of root" in {
    assert(FolderTree(FolderPath.Root, Paths, toPath _) == RootFolder)
  }

  "FolderTree of subfolder" in {
    assert(FolderTree(FolderPath("/x"), SubfolderPaths, toPath _) == SubFolder)
  }
}

object FolderTreeTest {

  private case class J(string: String)

  private def toPath(j: J) = JobPath(j.string)

  private val SubfolderPaths = List(
    J("/x/x-a"),
    J("/x/x-b"),
    J("/x/x-y/x-y-a"))

  private val Paths =
    J("/a") ::
    J("/b") ::
    SubfolderPaths

  private val SubFolder =
    FolderTree(FolderPath("/x"),
      leafs = List(
        Leaf("x-a", J("/x/x-a")),
        Leaf("x-b", J("/x/x-b"))),
      subfolders = List(
        FolderTree(FolderPath("/x/x-y"),
          leafs = List(
            Leaf("x-y-a", J("/x/x-y/x-y-a"))),
          subfolders = Nil)))

  private val RootFolder =
    FolderTree(FolderPath("/"),
      leafs = List(
        Leaf("a", J("/a")),
        Leaf("b", J("/b"))),
      subfolders = List(SubFolder))
}
