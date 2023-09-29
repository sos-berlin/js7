package js7.data_for_java.workflow.position

import cats.syntax.show.*
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.workflow.position.BranchPath
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.vavr.Standards.VEither
import js7.data_for_java.vavr.VavrConverters.RichVavrOption
import scala.jdk.CollectionConverters.*
import js7.data.workflow.position.BranchPath.syntax.*

final case class JBranchPath(asScala: BranchPath) extends JavaWrapper:
  type AsScala = BranchPath

  def normalize: JBranchPath =
    JBranchPath(BranchPath.normalize(asScala))

  def toList: java.util.List[Any] =
    asScala.toFlatSeq.asJava

  override def toString = asScala.show

object JBranchPath:
  val empty: JBranchPath = JBranchPath(BranchPath.empty)

  @javaApi
  @Nonnull
  def fromList(@Nonnull branchPath: java.util.List[Any]): VEither[Problem, JBranchPath] =
    BranchPath.fromSeq(branchPath.asScala.toSeq).map(apply).toVavr
