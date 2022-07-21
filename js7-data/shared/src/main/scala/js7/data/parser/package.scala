package js7.data

package object parser
{
  private val logger = scribe.Logger[this.type]

  // Scala 2 only
  val UseFastparse = sys.props.get("js7.parser") match {
    case None | Some("cats") =>
      false

    case Some("fastparse") =>
      logger.warn("js7.parser=fastparse - Using legacy fastparse library")
      true

    case Some(x) =>
      val msg = s"Unknown value for system property js7.parser=$x. Allowed are cats and fastparse"
      logger.error(msg)
      sys.error(msg)
  }
}
