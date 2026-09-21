# Why FileDescriptor.close blocks after interrupt() under Windows.

Asking Anthropic Claude:

don't look at my code. Look only into the following stacktrace. Why does the thread block?

```
"js7-0" #111 [6416] daemon prio=5 os_prio=0 cpu=187.50ms elapsed=68.63s tid=0x000002061ddca670 nid=6416 runnable  [0x0000006e341fe000]
   java.lang.Thread.State: RUNNABLE
        at java.io.FileDescriptor.close0(java.base@21.0.2/Native Method)
        at java.io.FileDescriptor.close(java.base@21.0.2/FileDescriptor.java:304)
        - locked <0x00000000f1c14750> (a java.io.FileDescriptor)
        at java.io.FileInputStream$1.close(java.base@21.0.2/FileInputStream.java:526)
        at java.io.FileDescriptor.closeAll(java.base@21.0.2/FileDescriptor.java:362)
        - locked <0x00000000f1c14750> (a java.io.FileDescriptor)
        at java.io.FileInputStream.close(java.base@21.0.2/FileInputStream.java:524)
        at java.nio.channels.Channels$ReadableByteChannelImpl.implCloseChannel(java.base@21.0.2/Channels.java:335)
        at java.nio.channels.spi.AbstractInterruptibleChannel$1.interrupt(java.base@21.0.2/AbstractInterruptibleChannel.java:163)
        - locked <0x00000000f37f0458> (a java.lang.Object)
        at java.lang.Thread.interrupt(java.base@21.0.2/Thread.java:1722)
        - locked <0x00000000f1c01a90> (a java.lang.Object)
        at cats.effect.IOFiberPlatform.busyWait$1(IOFiberPlatform.scala:128)
        at cats.effect.IOFiberPlatform.$anonfun$2(IOFiberPlatform.scala:138)
        at cats.effect.IOFiberPlatform$$Lambda/0x00000205cbe215b8.apply(Unknown Source)
        at cats.effect.IOFiber.runLoop(IOFiber.scala:429)
        at cats.effect.IOFiber.asyncContinueCanceledWithFinalizerR(IOFiber.scala:1421)
        at cats.effect.IOFiber.run(IOFiber.scala:126)
        at cats.effect.unsafe.WorkerThread.run(WorkerThread.scala:935)
```

# Claude's response

Reading only that stack:

## What the thread is doing

It is a Cats Effect worker finishing the **cancellation of an `IO.interruptible`** block (`asyncContinueCanceledWithFinalizerR` → `IOFiberPlatform.busyWait`). CE's `interruptible` cancellation loop repeatedly calls `Thread.interrupt()` on the thread that runs the blocking body, until that body acknowledges.

The catch is that `Thread.interrupt()` is not a cheap flag set here. The target thread is parked inside an `InterruptibleChannel` operation, so it has registered an interruptor via `AbstractInterruptibleChannel.begin()`. `Thread.interrupt()` runs that interruptor **synchronously, on the interrupting thread** (this one), while holding the target's `interruptLock` (`0xf1c01a90`). The interruptor's job is to close the channel:

```
Thread.interrupt                       ← holds target's interruptLock
 └ AbstractInterruptibleChannel$1.interrupt
    └ Channels$ReadableByteChannelImpl.implCloseChannel   ← Channels.newChannel(someInputStream)
       └ FileInputStream.close → FileDescriptor.closeAll  ← holds FileDescriptor monitor 0xf1c14750
          └ FileDescriptor.close0  (native)               ← stuck here
```

## Why `close0` doesn't return

The fact that the interruptor ran at all proves another thread is *currently inside* a read on that same channel/descriptor — that's the only state in which the interruptor is registered. So the OS is being asked to close a descriptor that has a blocking read in flight.

- On Windows (which this dump is from — `nid=6416`, that tid format), `CloseHandle` on a pipe handle with a pending synchronous `ReadFile` does not abort the read; it waits for the file object's pending I/O to drain.
- The pending read can only finish when the writer at the other end produces data or closes it (e.g. the child process writes or exits).

So this thread sits in the kernel until the peer does something. `cpu=187.50ms` over `elapsed=68.63s` confirms it's genuinely blocked, not spinning — `RUNNABLE` just means "in a native method", where the JVM can't observe the block.

There's also a latent lock cycle: when the reading thread does return from the read, it runs `end()`, which needs the same `interruptLock` that this thread is holding, plus the `FileDescriptor` monitor it also holds.

## Knock-on effects

- `fiber.cancel` never completes, so anything awaiting it hangs.
- One CE **compute** worker is consumed indefinitely — `busyWait` isn't run in a blocking region, so the pool loses a thread.
- Whoever else touches `0xf1c01a90` (any `interrupt()`/`isInterrupted()` on that thread) or `0xf1c14750` (closing/registering that fd) blocks behind it.

## What avoids it

Don't let cancellation be the thing that closes the descriptor:

- Use `IO.blocking` (not `interruptible`) for reads on a pipe, and close the stream from a **separate** fiber on cancel (`startAndForget`), so the canceller doesn't wait for `close`.
- Or don't wrap a process `InputStream` in `Channels.newChannel` — the plain `InputStream.read` path has no interruptor, so `Thread.interrupt()` stays a flag set.
- Or make the peer end terminate first (kill/await the child process), then close and let the read return EOF.
