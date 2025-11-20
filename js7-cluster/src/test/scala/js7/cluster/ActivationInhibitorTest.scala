package js7.cluster

import cats.effect.testkit.TestControl
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Outcome}
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.cluster.ActivationConsentChecker.Consent
import js7.cluster.ActivationInhibitor.{Active, Inhibited, Initial, Passive}

final class ActivationInhibitorTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  "startActive, startPassive" - {
    lazy val inhibitor = startActivationInhibitor.await(99.s)

    "state is Initial" in:
      TestControl.executeEmbed:
        for result <- inhibitor.state yield
          assert(result == Some(Initial))

    "startActive" in:
      TestControl.executeEmbed:
        for result <- inhibitor.startActive yield
          assert(result == ())

    "Following startActive is rejected" in:
      TestControl.executeEmbed:
        for result <- inhibitor.startActive.attempt yield
          assert(result.isLeft)

    "Following startPassive is rejected" in:
      TestControl.executeEmbed:
        for result <- inhibitor.startPassive.attempt yield
          assert(result.isLeft)

    "state is Active" in:
      TestControl.executeEmbed:
        for result <- inhibitor.state yield
          assert(result == Some(Active))
  }

  "tryActivate" - {
    lazy val inhibitor = startActivationInhibitor.await(99.s)
    lazy val activation = succeedingActivation(inhibitor, Right(Consent.Given))

    "first" in:
      TestControl.executeEmbed:
        val activated = Atomic(false)
        for
          _ <- activation.<*(IO(activated := true)).start
          _ <- IO.sleep(1.ms)
          _ = assert(!activated.get)
          _ <- for s <- inhibitor.state yield assert(s == None)

          _ <- IO.sleep(1.s)
          _ = assert(activated.get)
          _ <- for s <- inhibitor.state yield assert(s == Some(Active))
        yield
          succeed

    "second activation is allowed" in:
      // That means, the current activation is acknowledged
      TestControl.executeEmbed:
        val activated = Atomic(false)
        for
          a <- activation.<*(IO(activated := true)).start
          _ <- IO.sleep(1.s + 1.ns/*???*/)
          _ = assert(activated.get)
          _ <- for a <- a.joinStd yield assert(a == Right(Consent.Given))
          state <- inhibitor.state
        yield
          assert(state == Some(Active))
  }

  "tryActivate but body rejects with Right(false)" in:
    TestControl.executeEmbed:
      val activated = Atomic(false)
      for
        inhibitor <- startActivationInhibitor
        activation = succeedingActivation(inhibitor, Right(Consent.Rejected))
        a <- activation.<*(IO(activated := true)).start
        _ <- IO.sleep(1.ms)
        _ = assert(!activated.get)
        _ <- for s <- inhibitor.state yield assert(s == None)

        _ <- IO.sleep(1.s)
        _ = assert(activated.get)
        _ <- for a <- a.joinStd yield assert(a == Right(Consent.Rejected))
        _ <- for s <- inhibitor.state yield assert(s == Some(Passive))
      yield
        succeed

  "tryActivate but body returns Left(problem)" in:
    TestControl.executeEmbed:
      lazy val inhibitor = startActivationInhibitor.await(99.s)
      lazy val activation = succeedingActivation(inhibitor, Left(Problem("PROBLEM")))
      val activated = Atomic(false)
      for
        a <- activation.<*(IO(activated := true)).start
        _ <- IO.sleep(1.ms)
        _ = assert(!activated.get)
        _ <- for s <- inhibitor.state yield assert(s == None)

        _ <- IO.sleep(1.s)
        _ = assert(activated.get)
        _ <- for a <- a.joinStd yield assert(a == Left(Problem("PROBLEM")))
        _ <- for s <- inhibitor.state yield assert(s == Some(Initial))
      yield
        succeed

  "tryActivate with failed activation" - {
    lazy val inhibitor = startActivationInhibitor.await(99.s)

    "first" in:
      TestControl.executeEmbed:
        val passivated = Atomic(false)
        val activated = Atomic(false)
        for
          a <- inhibitor.startPassive.<*(IO(passivated := true)).start
          _ <- IO.sleep(1.ms)
          _ = assert(passivated.get)
          _ <- for a <- a.joinStd yield assert(a == ())

          b <- failingActivation(inhibitor).<*(IO(activated := true)).start
          _ <- IO.sleep(1.ms)
          _ = assert(!activated.get)
          _ <- for s <- inhibitor.state yield assert(s == None)

          _ <- IO.sleep(1.s)
          _ <- b.join.map:
            case Outcome.Errored(t) if t.toString == "java.lang.RuntimeException: TEST" => succeed
            case o => fail(s"Unexpected $o")
          _ <- for s <- inhibitor.state yield assert(s == Some(Passive))
        yield
          succeed

    "again with succeeding activation" in:
      TestControl.executeEmbed:
        for
          a <- succeedingActivation(inhibitor, Right(Consent.Given))
          _ = assert(a == Right(Consent.Given))
          _ <- for s <- inhibitor.state yield assert(s == Some(Active))
        yield
          succeed
  }

  "inhibitActivation" in:
    TestControl.executeEmbed:
      for
        inhibitor <- startActivationInhibitor
        _ <- inhibitor.startPassive
        inhibited <- inhibitor.inhibitActivation(2.s)
        _ = assert(inhibited)
        _ <- for s <- inhibitor.state yield assert(s == Some(Inhibited(depth = 1)))
        b <- succeedingActivation(inhibitor, Right(Consent.Given))

        // While inhibition is in effect
        _ <- IO.sleep(1.s)
        _ <- for s <- inhibitor.state yield assert(s == Some(Inhibited(depth = 1)))
        _ <- IO.sleep(1.s + 1.ns)
        _ <- for s <- inhibitor.state yield assert(s == Some(Passive))

        // After inhibition has timed out, activation starts
        a <- succeedingActivation(inhibitor, Right(Consent.Given)).start
        _ <- IO.sleep(1.ms)
        _ <- for o <- a.joinStd.timeoutTo(1.ms, IO("TIMEOUT")) yield assert(o == "TIMEOUT")

        a <- a.joinStd
        _ = assert(a == Right(Consent.Given))
        _ <- for s <- inhibitor.state yield assert(s == Some(Active))

        // inhibitActivation returns false if state is active" in
        inhibiting <- inhibitor.inhibitActivation(2.s).start
        _ <- for o <- inhibiting.joinStd.timeout(1.ms) yield assert(!o)
      yield
        succeed

  "inhibitActivation waits when currently activating" in:
    TestControl.executeEmbed:
      for
        inhibitor <- startActivationInhibitor
        _ <- inhibitor.startPassive.timeout(1.ms)
        b <- succeedingActivation(inhibitor, Right(Consent.Given)).start

        _ <- IO.sleep(1.ms)
        inhibiting <- inhibitor.inhibitActivation(2.s).start
        _ <- for o <- inhibiting.joinStd.timeoutTo(1.ms, IO("TIMEOUT")) yield assert(o == "TIMEOUT")

        _ <- IO.sleep(1.s)
        _ <- for b <- b.joinStd.timeout(1.ms) yield assert(b == Right(Consent.Given))
        _ <- for o <- inhibiting.joinStd.timeout(1.ms) yield assert(!o)
        _ <- for s <- inhibitor.state yield assert(s == Some(Active))

        _ <- IO.sleep(2.s)
        _ <- for s <- inhibitor.state yield assert(s == Some(Active))
      yield succeed

  "inhibit while inhibiting" in:
    TestControl.executeEmbed:
      for
        inhibitor <- startActivationInhibitor
        _ <- inhibitor.startPassive.timeout(1.ms)
        inhibited <- inhibitor.inhibitActivation(2.s).timeout(1.ms)
        _ = assert(inhibited)

        _ <- IO.sleep(1.s)
        _ <- for s <- inhibitor.state yield assert(s == Some(Inhibited(depth = 1)))

        inhibited <- inhibitor.inhibitActivation(2.s).timeout(1.ms)
        _ = assert(inhibited)
        _ <- for s <- inhibitor.state yield assert(s == Some(Inhibited(depth = 2)))

        _ <- IO.sleep(1.s + 1.ns)
        _ <- for s <- inhibitor.state yield assert(s == Some(Inhibited(depth = 1)))

        _ <- IO.sleep(1.s)
        _ <- for s <- inhibitor.state yield assert(s == Some(Passive))
      yield
        succeed

  private def startActivationInhibitor: IO[ActivationInhibitor] =
    ActivationInhibitor.resource().allocated.map(_._1)

  private def succeedingActivation(inhibitor: ActivationInhibitor, bodyResult: Checked[Consent])
  : IO[Checked[Consent]] =
    inhibitor.tryToActivate:
      IO.sleep(1.s).as(bodyResult)

  private def failingActivation(inhibitor: ActivationInhibitor): IO[Checked[Consent]] =
    inhibitor.tryToActivate:
      IO.sleep(1.s) *> IO.raiseError(new RuntimeException("TEST"))
