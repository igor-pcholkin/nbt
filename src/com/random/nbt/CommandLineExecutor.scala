package com.random.nbt

import com.typesafe.scalalogging.LazyLogging
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object CommandLineExecutor extends LazyLogging {
  def execute(cmdLine: String)(implicit context: Context) = {
    import sys.process._
    val refinedCmdLine = context.resolveVarsIn(cmdLine)
    logger.info(s"Executing command: $refinedCmdLine")
    val workingDir = context.getString("projectDir", "currentDir").getOrElse(".")
    Try {
      Process(refinedCmdLine, new java.io.File(workingDir)).!!
    }
    match {
      case Success(output: String) =>
        if (output.nonEmpty)
          println(output)
        true
      case Failure(ex) =>
        logger.error(ex.getMessage)
        false
    }
  }
}
