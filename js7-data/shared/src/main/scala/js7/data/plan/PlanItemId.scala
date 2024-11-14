package js7.data.plan

import js7.base.generic.GenericString
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.item.UnsignedSimpleItemPath

final case class PlanItemId private(string: String)
  extends GenericString, UnsignedSimpleItemPath:

  protected type Self = PlanItemId

  val companion: PlanItemId.type = PlanItemId


object PlanItemId extends
  GenericString.NonEmpty[PlanItemId],
  UnsignedSimpleItemPath.Companion[PlanItemId]:

  type Item = PlanItem

  val Global: PlanItemId = new PlanItemId("Global")

  protected def unchecked(string: String) =
    new PlanItemId(string)
