package js7.base.serial

/**
  * @author Joacim Zschimmer
  */
trait PathAndParameterSerializable[A]:
  def toPathAndParameters(a: A): (String, Map[String, String])
  def fromPathAndParameters(pathAndParameters: (String, Map[String, String])): A

object PathAndParameterSerializable:

  def toPathAndParameters[A: PathAndParameterSerializable](a: A): (String, Map[String, String]) =
    implicitly[PathAndParameterSerializable[A]].toPathAndParameters(a)

  def fromPathAndParameters[A: PathAndParameterSerializable](pathAndParameters: (String, Map[String, String])): A =
    implicitly[PathAndParameterSerializable[A]].fromPathAndParameters(pathAndParameters)
