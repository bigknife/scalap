package bigknife.scalap.interpreter
package service

import bigknife.scalap.ast.service.LogService
import org.slf4j.LoggerFactory

class LogServiceHandler extends LogService.Handler[Stack] {
  private val logger = LoggerFactory.getLogger("scalap.ast")

  override def debug(message: String, tag: Option[String], cause: Option[Throwable]): Stack[Unit] =
    Stack {
      val msg = tag.map(t => s"[$t] $message").getOrElse(message)
      if (cause.isDefined) logger.debug(msg, cause.get)
      else logger.debug(msg)
    }

  override def info(message: String, tag: Option[String], cause: Option[Throwable]): Stack[Unit] =
    Stack {
      val msg = tag.map(t => s"[$t] $message").getOrElse(message)
      if (cause.isDefined) logger.info(msg, cause.get)
      else logger.info(msg)
    }

  override def warn(message: String, tag: Option[String], cause: Option[Throwable]): Stack[Unit] =
    Stack {
      val msg = tag.map(t => s"[$t] $message").getOrElse(message)
      if (cause.isDefined) logger.warn(msg, cause.get)
      else logger.warn(msg)
    }

  override def error(message: String, tag: Option[String], cause: Option[Throwable]): Stack[Unit] =
    Stack {
      val msg = tag.map(t => s"[$t] $message").getOrElse(message)
      if (cause.isDefined) logger.error(msg, cause.get)
      else logger.error(msg)
    }
}

object LogServiceHandler {
  private val _inst = new LogServiceHandler
  trait Implicits {
    implicit val logService: LogServiceHandler = _inst
  }
}
