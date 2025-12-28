package js7.core.command

import js7.base.auth.SimpleUser
import js7.base.scalasource.ScalaSourceLocation
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkohttp.StandardDirectives.requestSource
import org.apache.pekko.http.scaladsl.server.{Directive1, Route}
import org.jetbrains.annotations.TestOnly

final case class CommandMeta(user: SimpleUser, source: String):
  override def toString = s"$user${source.nonEmpty ?? s"($source)"}"


object CommandMeta:

  def system(source: String): CommandMeta =
    CommandMeta(SimpleUser.System, source = source)

  @TestOnly
  def test(using loc: ScalaSourceLocation): CommandMeta =
    CommandMeta(SimpleUser.Test, source = loc.toString)

  @TestOnly
  def test(source: String): CommandMeta =
    CommandMeta(SimpleUser.Test, source = source)

  object pekkoDirectives:
    def commandMeta(user: SimpleUser): Directive1[CommandMeta] =
      new Directive1[CommandMeta]:
        def tapply(inner: Tuple1[CommandMeta] => Route) =
          requestSource: source =>
            inner(Tuple1(CommandMeta(user, source)))

