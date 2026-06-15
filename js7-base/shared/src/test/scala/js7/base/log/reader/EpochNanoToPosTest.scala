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
    assert(nanoToPos.toOpaquePos(EpochNano(1)) == OpaquePos(0))

    nanoToPos.add(EpochNano(10), OpaquePos(1000), 1111)
    nanoToPos.add(EpochNano(20), OpaquePos(2000), 2222)
    nanoToPos.add(EpochNano(30), OpaquePos(3000), 3333)
    assert(!nanoToPos.isEmpty)
    assert(nanoToPos.length == 3)
    assert(nanoToPos.internalSize == 16)

    assert(nanoToPos.toOpaquePos(EpochNano(0)) == OpaquePos(0))
    assert(nanoToPos.toOpaquePos(EpochNano(9)) == OpaquePos(0))
    assert(nanoToPos.toOpaquePos(EpochNano(10)) == OpaquePos(1000))
    assert(nanoToPos.toOpaquePos(EpochNano(11)) == OpaquePos(1000))
    assert(nanoToPos.toOpaquePos(EpochNano(20)) == OpaquePos(2000))
    assert(nanoToPos.toOpaquePos(EpochNano(21)) == OpaquePos(2000))
    assert(nanoToPos.toOpaquePos(EpochNano(30)) == OpaquePos(3000))
    assert(nanoToPos.toOpaquePos(EpochNano(31)) == OpaquePos(3000))

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
