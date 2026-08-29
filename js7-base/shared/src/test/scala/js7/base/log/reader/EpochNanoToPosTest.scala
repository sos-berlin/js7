package js7.base.log.reader

import js7.base.io.OpaquePos
import js7.base.test.OurTestSuite
import js7.base.time.EpochNano

final class EpochNanoToPosTest extends OurTestSuite:

  "EpochNanoToPos" in:
    val nanoToPos = new EpochNanoToPos(initialSize = 1)
    assert(nanoToPos.isEmpty)
    assert(nanoToPos.length == 0)
    assert(nanoToPos.internalSize == 1)
    assert(nanoToPos.lastEpochNano == EpochNano.MinValue)
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(1)) == 0 -> OpaquePos(0))

    nanoToPos.add(EpochNano(10), OpaquePos(1000), 1111)
    nanoToPos.add(EpochNano(20), OpaquePos(2000), 2222)
    nanoToPos.add(EpochNano(30), OpaquePos(3000), 3333)
    assert(!nanoToPos.isEmpty)
    assert(nanoToPos.length == 3)
    assert(nanoToPos.internalSize == 16)

    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(0)) == 0 -> OpaquePos(0))
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(9)) == 0 -> OpaquePos(0))
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(10)) == 1111 -> OpaquePos(1000))
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(11)) == 1111 -> OpaquePos(1000))
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(20)) == 2222 -> OpaquePos(2000))
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(21)) == 2222 -> OpaquePos(2000))
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(30)) == 3333 -> OpaquePos(3000))
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(31)) == 3333 -> OpaquePos(3000))

    assert(nanoToPos.posToChunkPosAndOpaquePos(0) == 0 -> OpaquePos(0))
    assert(nanoToPos.posToChunkPosAndOpaquePos(1111) == 1111 -> OpaquePos(1000))
    assert(nanoToPos.posToChunkPosAndOpaquePos(2222) == 2222 -> OpaquePos(2000))
    assert(nanoToPos.posToChunkPosAndOpaquePos(3333) == 3333 -> OpaquePos(3000))

    assert(nanoToPos.posToChunkPosAndOpaquePos(1000) == 0 -> OpaquePos(0))
    assert(nanoToPos.posToChunkPosAndOpaquePos(2000) == 1111 -> OpaquePos(1000))
    assert(nanoToPos.posToChunkPosAndOpaquePos(3000) == 2222 -> OpaquePos(2000))
    assert(nanoToPos.posToChunkPosAndOpaquePos(4000) == 3333 -> OpaquePos(3000))

    nanoToPos.shrink()
    assert(nanoToPos.length == 3)
    assert(nanoToPos.internalSize == 1 + 3)
