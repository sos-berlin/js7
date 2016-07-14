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
    assert(FolderTree.fromAny(FolderPath.Root, Paths, toPath) == RootFolder)
  }

  "FolderTree of subfolder" in {
    assert(FolderTree.fromAny(FolderPath("/x"), SubfolderPaths, toPath) == SubFolder)
  }

  "ordering" in {
    val a = FolderTree(FolderPath("/a/a"), Nil, Nil)
    val b = FolderTree(FolderPath("/a/b"), Nil, Nil)
    val c = FolderTree(FolderPath("/a/c"), Nil, Nil)
    val d = FolderTree(FolderPath("/a/d"), Nil, Nil)
    assert(Vector(c, a, d, b).sorted == Vector(a, b, c, d))
  }
}

object FolderTreeTest {

  private case class A(string: String)

  private def toPath(a: A) = JobPath(a.string)

  private val SubfolderPaths = List(
    A("/x/x-a"),
    A("/x/x-b"),
    A("/x/x-y/x-y-a"))

  private val Paths =
    A("/a") ::
    A("/b") ::
    SubfolderPaths

  private val SubFolder =
    FolderTree(FolderPath("/x"),
      leafs = List(
        Leaf("x-a", A("/x/x-a")),
        Leaf("x-b", A("/x/x-b"))),
      subfolders = List(
        FolderTree(FolderPath("/x/x-y"),
          leafs = List(
            Leaf("x-y-a", A("/x/x-y/x-y-a"))),
          subfolders = Nil)))

  private val RootFolder =
    FolderTree(FolderPath("/"),
      leafs = List(
        Leaf("a", A("/a")),
        Leaf("b", A("/b"))),
      subfolders = List(SubFolder))
}
