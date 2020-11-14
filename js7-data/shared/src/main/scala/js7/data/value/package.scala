package js7.data

package object value
{
  type NamedValues = Map[String, Value]

  object NamedValues
  {
    val empty = Map.empty[String, Value]
  }
}
