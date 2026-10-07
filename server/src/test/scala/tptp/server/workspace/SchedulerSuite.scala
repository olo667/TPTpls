package tptp.server.workspace

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

class SchedulerSuite extends munit.FunSuite {
  private def scheduler(delay: Long) =
    Scheduler(() => delay, Executors.newSingleThreadScheduledExecutor(), Executors.newSingleThreadExecutor())

  test("rapid requests for one key run the task once") {
    val s = scheduler(50)
    val runs = AtomicInteger()
    val done = CountDownLatch(1)
    (1 to 5).foreach(_ => s.schedule("a") { runs.incrementAndGet(); done.countDown() })
    assert(done.await(2, TimeUnit.SECONDS))
    Thread.sleep(200)
    assertEquals(runs.get, 1)
    s.shutdown()
  }

  test("a request made while the task runs leads to exactly one more run after it") {
    val s = scheduler(0)
    val started = CountDownLatch(1)
    val release = CountDownLatch(1)
    val runs = AtomicInteger()
    val second = CountDownLatch(1)
    s.schedule("a") { runs.incrementAndGet(); started.countDown(); release.await(5, TimeUnit.SECONDS) }
    assert(started.await(2, TimeUnit.SECONDS))
    s.schedule("a") { runs.incrementAndGet(); second.countDown() }
    release.countDown()
    assert(second.await(2, TimeUnit.SECONDS))
    Thread.sleep(100)
    assertEquals(runs.get, 2)
    s.shutdown()
  }

  test("different keys do not affect each other") {
    val s = scheduler(10)
    val done = CountDownLatch(2)
    s.schedule("a")(done.countDown())
    s.schedule("b")(done.countDown())
    assert(done.await(2, TimeUnit.SECONDS))
    s.shutdown()
  }
}
