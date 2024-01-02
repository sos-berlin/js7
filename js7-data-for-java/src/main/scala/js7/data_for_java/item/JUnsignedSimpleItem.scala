package js7.data_for_java.item

import java.util.Optional
import javax.annotation.Nonnull
import js7.data.item.{ItemRevision, SimpleItemPath, UnsignedSimpleItem}

trait JUnsignedSimpleItem:
  @Nonnull
  def asScala: UnsignedSimpleItem

  @Nonnull
  def path: SimpleItemPath

  @Nonnull
  def withRevision(revision: Optional[ItemRevision]): JUnsignedSimpleItem
