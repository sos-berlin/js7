package js7.subagent.client

final class NumberedQueueHolderTest extends AsyncFreeSpec
{
  private val subagentId = SubagentId("A")
  private val keeper = new NumberedQueueHolder[SubagentId, X]

  private def observe(after: Long, take: Int): Checked[List[Numbered[X]]] =
    keeper
      .observable(subagentId, after)
      .flatMap(_.traverse(_
        .take(take)
        .toListL))
      .await(99.s)

  "Unknown SubagentId" in {
    assert(observe(0, take = 0) == Left(UnknownKeyProblem("SubagentId", subagentId)))
  }

  "Known SubagentId" in {
    keeper
      .addKey(subagentId)
      .map(_ => assert(observe(0, take = 0) == Right(Nil)))
      .runToFuture
  }

  "Enqueue some values" in {
    keeper
      .enqueue(subagentId, X("a"))
      .map(_.orThrow)
      .map(_ => assert(observe(0, take = 1) == Right(List(Numbered(1, X("a"))))))
      .runToFuture
  }

  "releaseUntil" in {
    keeper.enqueue(subagentId, X("b"))
      .map(_.orThrow)
      .flatMap(_ => keeper.releaseUntil(subagentId, 1))
      .map(_.orThrow)
      .map(_ =>
        assert(observe(0, take = 1) ==
          Left(Problem("Unknown Numbered[NumberedQueueHolderTest::X]: #0"))))
      .map(_ =>
        assert(keeper.releaseUntil(subagentId, 3).await(99.s) ==
          Left(Problem("releaseUntil(3) > last.number ?"))))
      .runToFuture
  }
}

object NumberedQueueHolderTest {
  private case class X(string: String)
}
