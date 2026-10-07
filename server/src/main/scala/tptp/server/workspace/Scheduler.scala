package tptp.server.workspace

import java.util.concurrent.{ExecutorService, ScheduledExecutorService, ScheduledFuture, TimeUnit}
import scala.collection.mutable

/** Debounces work per key and runs it on `worker`: requests within the delay collapse into one run. */
final class Scheduler(delayMs: () => Long, timer: ScheduledExecutorService, worker: ExecutorService) {
  private val pending = mutable.Map.empty[String, ScheduledFuture[?]]

  def schedule(key: String)(task: => Unit): Unit = synchronized {
    pending.remove(key).foreach(_.cancel(false))
    lazy val future: ScheduledFuture[?] = timer.schedule(
      new Runnable {
        def run(): Unit = {
          Scheduler.this.synchronized { if (pending.get(key).exists(_ eq future)) pending.remove(key) }
          worker.execute(() => task)
        }
      },
      delayMs(),
      TimeUnit.MILLISECONDS,
    )
    pending(key) = future
  }

  def shutdown(): Unit = {
    timer.shutdownNow()
    worker.shutdownNow()
  }
}
