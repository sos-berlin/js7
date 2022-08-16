package js7.data.folder

import js7.base.test.OurTestSuite
import js7.data.folder.FolderPathTest.*
import js7.data.item.VersionedItemPath

/**
  * @author Joacim Zschimmer
  */
final class FolderPathTest extends OurTestSuite
{
  "subfolder" in {
    assert(FolderPath("").subfolder("x") == FolderPath(""))
    assert(FolderPath("a").subfolder("x") == FolderPath("a/x"))
    assert(FolderPath("a/b").subfolder("x") == FolderPath("a/b/x"))
    intercept[IllegalArgumentException] { FolderPath("") subfolder "/" }
    intercept[IllegalArgumentException] { FolderPath("") subfolder "/x" }
    intercept[IllegalArgumentException] { FolderPath("") subfolder "x/" }
    intercept[IllegalArgumentException] { FolderPath("") subfolder "x/y" }
  }

  "isParentOf" in {
    assert(FolderPath.Root.isParentOf(FolderPath("a")))
    assert(!FolderPath.Root.isParentOf(FolderPath.Root))
    assert(!FolderPath("a").isParentOf(FolderPath("a")))
    assert(!FolderPath("a").isParentOf(TestPath("x")))
    assert(!FolderPath("a/b").isParentOf(TestPath("a")))
    assert(!FolderPath("a/b").isParentOf(TestPath("a/b")))
    assert(FolderPath("").isParentOf(TestPath("x")))
    assert(FolderPath("a/b").isParentOf(TestPath("a/b/c")))
    assert(!FolderPath("a/b").isParentOf(TestPath("a/b/c/d")))
  }

  "isAncestorOf" in {
    assert(FolderPath.Root isAncestorOf FolderPath.Root)
    assert(FolderPath("a") isAncestorOf FolderPath("a"))
    assert(!FolderPath("a").isAncestorOf(TestPath("x")))
    assert(!FolderPath("a/b").isAncestorOf(TestPath("a")))
    assert(!FolderPath("a/b").isAncestorOf(TestPath("a/b")))
    assert(FolderPath("") isAncestorOf TestPath("x"))
    assert(FolderPath("a/b") isAncestorOf TestPath("a/b/c"))
    assert(FolderPath("a/b") isAncestorOf TestPath("a/b/c/d"))
  }

  "resolve" in {
    assert(FolderPath("default").resolve[TestPath]("a") == TestPath("default/a"))
    //assert(FolderPath("default").resolve[TestPath]("./a") == TestPath("default/a"))
    //assert(FolderPath("default").resolve[TestPath]("./a") == TestPath("default/a"))
    //assert(FolderPath.Root.resolve[TestPath]("./a") == TestPath("a"))
    assert(FolderPath("default/x").resolve[TestPath]("a/b") == TestPath("default/x/a/b"))
    //assert(FolderPath("default/x").resolve[TestPath]("./a/b") == TestPath("default/x/a/b"))
    //assert(FolderPath("default/x").resolve[TestPath]("./a/b") == TestPath("default/x/a/b"))
    //intercept[Exception] { FolderPath("").resolve[TestPath](")./a") }
  }

  "name" in {
    assert(FolderPath("").name == "")
    assert(FolderPath("A").name == "A")
    assert(FolderPath("A/B").name == "B")
  }

  "withTrailingSlash" in {
    assert(FolderPath.checked("FOLDER/").isLeft)
    assert(FolderPath("FOLDER").withTrailingSlash == "FOLDER/")
  }

  "FolderPath.parentOf" in {
    assert(FolderPath.parentOf(TestPath("a")) == FolderPath.Root)
    assert(FolderPath.parentOf(TestPath("folder/a")) == FolderPath("folder"))
    assert(FolderPath.parentOf(TestPath("x/folder/a")) == FolderPath("x/folder"))
    intercept[IllegalStateException] { FolderPath.parentOf(FolderPath.Root) }
  }
}

private object FolderPathTest
{
  private case class TestPath(string: String) extends VersionedItemPath {
    def companion = TestPath
  }

  private object TestPath extends VersionedItemPath.Companion[TestPath] {
    protected def unchecked(string: String) = new TestPath(string)
  }
}
