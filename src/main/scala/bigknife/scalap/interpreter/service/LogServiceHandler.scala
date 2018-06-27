package bigknife.scalap.interpreter
package service

import bigknife.scalap.ast.service.LogService
import org.slf4j.LoggerFactory

class LogServiceHandler extends LogService.Handler[Stack]{
  private val logger = LoggerFactory.getLogger("scalap.ast")

  override def debug(message: String, cause: Option[Throwable]): Stack[Unit] = Stack {
    if (cause.isDefined) logger.debug(message, cause.get)
    else logger.debug(message)
  }

  override def info(message: String, cause: Option[Throwable]): Stack[Unit] = Stack {
    if (cause.isDefined) logger.info(message, cause.get)
    else logger.info(message)
  }

  override def warn(message: String, cause: Option[Throwable]): Stack[Unit] = Stack {
    if (cause.isDefined) logger.warn(message, cause.get)
    else logger.warn(message)
  }

  override def error(message: String, cause: Option[Throwable]): Stack[Unit] = Stack {
    if (cause.isDefined) logger.error(message, cause.get)
    else logger.error(message)
  }
}

object LogServiceHandler {
  trait Implicits {
    implicit val logService: LogServiceHandler = new LogServiceHandler
  }
}