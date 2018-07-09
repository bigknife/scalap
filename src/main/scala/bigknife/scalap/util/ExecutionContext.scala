package bigknife.scalap.util

import java.util.concurrent.{ExecutorService, Executors, ThreadFactory}

import org.slf4j.{Logger, LoggerFactory}

/**
  * execution context for scp rounded timer
  */
trait ExecutionContext {
  private val log: Logger = LoggerFactory.getLogger(getClass)

  private def tf(name: String): ThreadFactory =
    DefaultThreadFactory(
      name,
      (t: Thread, e: Throwable) => log.error(s"execution context uncaught exception in $t", e)
    )
  private val nominationEc: ExecutorService =
    Executors.newFixedThreadPool(1, tf("nominate-timer-ec"))
  private val ballotEc: ExecutorService = Executors.newFixedThreadPool(1, tf("ballot-timer-ec"))

  case class DefaultThreadFactory(name: String, handler: (Thread, Throwable) => Unit)
      extends ThreadFactory {
    override def newThread(r: Runnable): Thread = {
      val t = new Thread(r)
      t.setDaemon(true)
      t.setUncaughtExceptionHandler((t: Thread, e: Throwable) => handler(t, e))
      t
    }
  }

  private def submitTask(ec: ExecutorService, task: => Unit): Unit = {
    ec.submit(new Runnable {
      override def run(): Unit = task
    })
    ()
  }

  def submitNominationTask(task: => Unit): Unit = submitTask(nominationEc, task)
  def submitBallotTask(task: => Unit): Unit     = submitTask(ballotEc, task)
}
