package js7.base

object ProvisionalAssumptions:

  /** Use for Stream#chunkProvisional extension method.
   * TODO: The sources emits properly sized chunks because
   *  the source can estimate the memory usage.
   * Also, HTTP chunks may be used for multiple events and converted into FS2 chunks.
   */
  object streamChunks:
    private val elementSizeLimit = 8_000_000 // A big Workflow
    private val heapLimit = 100_000_000 // Bytes

    val elementsPerChunkLimit: Int = heapLimit / elementSizeLimit
