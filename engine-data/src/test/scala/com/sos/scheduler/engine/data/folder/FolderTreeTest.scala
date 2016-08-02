package com.sos.scheduler.engine.data.folder

import com.sos.scheduler.engine.data.folder.FolderTreeTest._
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import java.time.Instant.now
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import org.slf4j.LoggerFactory
import spray.json.DefaultJsonProtocol._
import spray.json._

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

  "JSON" in {
    implicit def aJsonFormat = jsonFormat1(A)
    // JSON object fields are unordered. So we use an array of key/value pairs for the leafs
    val json = """{
      "path": "/",
      "leafs": [
        { "content": "/a" },
        { "content": "/b" }
      ],
      "subfolders": [
        {
          "path": "/x",
          "leafs": [
            { "content": "/x/x-a" },
            { "content": "/x/x-b" }
          ],
          "subfolders": [
            {
              "path": "/x/x-y",
              "leafs": [
                { "content": "/x/x-y/x-y-a" }
              ],
              "subfolders": []
            }
          ]
        }
      ]
    }""".parseJson.asJsObject
    assert(RootFolder.toJson == json)
    assert(RootFolder == json.convertTo[FolderTree[A]])
  }

  if (false)
  "Speed" in {
    for (_ ← 1 to 3) {
      val start = now
      val n = 100000
      for (_ ← 1 to n) FolderTree.fromAny(FolderPath.Root, Paths, toPath)
      val duration = now.toEpochMilli - start.toEpochMilli
      logger.info(s"${duration}ms ${n * 1000L / duration}/s")
    }
  }
}

object FolderTreeTest {
  private val logger = LoggerFactory.getLogger(classOf[FolderTreeTest])

  private case class A(content: String)

  private def toPath(a: A) = JobPath(a.content)

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
        A("/x/x-a"),
        A("/x/x-b")),
      subfolders = List(
        FolderTree(FolderPath("/x/x-y"),
          leafs = List(
            A("/x/x-y/x-y-a")),
          subfolders = Nil)))

  private val RootFolder =
    FolderTree(FolderPath("/"),
      leafs = List(
        A("/a"),
        A("/b")),
      subfolders = List(SubFolder))
}
