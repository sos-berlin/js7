package js7.tests.controller.proxy

import js7.base.test.OurTestSuite
import js7.tests.testenv.DirectoryProviderForScalaTest

final class ScalaCompilerKnockOut extends OurTestSuite with DirectoryProviderForScalaTest:
  protected def agentPaths = Nil
  protected def items = Nil

  // The Scala compiler doest not crash when the line "controllerAdmission" becomes a comment.
  // Clean the js7-tests subproject before recompile:
  //
  // js7-tests/Test/clean
  // js7-tests/Test/compile


  controllerAdmission // <--- 💥


  /* 'controllerAdmission' crashs the Scala compiler:

    -- [E046] Cyclic Error: /Users/joa/dev/js7/js7-tests/src/test/scala/js7/tests/testenv/DirectoryProvider.scala:58:7
    58 |import scala.concurrent.duration.*
       |       ^
       |       Cyclic reference involving class DirectoryProvider
       |----------------------------------------------------------------------------
       | Explanation (enabled by `-explain`)
       |- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       | class DirectoryProvider is declared as part of a cycle which makes it impossible for the
       | compiler to decide upon DirectoryProvider's type.
       | To avoid this error, try giving DirectoryProvider an explicit type.
        ----------------------------------------------------------------------------
    Explanation
    ===========
    class DirectoryProvider is declared as part of a cycle which makes it impossible for the
    compiler to decide upon DirectoryProvider's type.
    To avoid this error, try giving DirectoryProvider an explicit type.

    (... some weird errors because DirectoryProvider did not compile correctly)

    exception occurred while typechecking /Users/joa/dev/js7/js7-tests/src/test/scala/js7/tests/cluster/controller/FailoverControllerClusterTest.scala

    An unhandled exception was thrown in the compiler.
    Please file a crash report here:
    https://github.com/lampepfl/dotty/issues/new/choose

       while compiling: /Users/joa/dev/js7/js7-tests/src/test/scala/js7/tests/cluster/controller/FailoverControllerClusterTest.scala
          during phase: typer
                  mode: Mode(ImplicitsEnabled)
       library version: version 2.13.12
      compiler version: version 3.3.3

    ## Exception when compiling 202 sources to /Users/joa/dev/js7/js7-tests/target/scala-3.3.3/test-classes
    java.lang.AssertionError: assertion failed: NoType
    scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:8)
    dotty.tools.dotc.core.Types$TypeBounds.<init>(Types.scala:5178)
    dotty.tools.dotc.core.Types$AliasingBounds.<init>(Types.scala:5257)
    dotty.tools.dotc.core.Types$TypeAlias.<init>(Types.scala:5279)
    dotty.tools.dotc.core.Types$TypeAlias$.apply(Types.scala:5316)
    dotty.tools.dotc.core.Types$Type.bounds(Types.scala:1756)
    ...
   */
