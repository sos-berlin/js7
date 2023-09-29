package js7.base.convert

import java.nio.file.{Path, Paths}

/**
  * @author Joacim Zschimmer
  */
object AsJava:

  def asAbsolutePath: As[String, Path] =
    As(o => StringAsPath(o).toAbsolutePath)

  implicit val StringAsPath: As[String, Path] =
    As(Paths.get(_))
