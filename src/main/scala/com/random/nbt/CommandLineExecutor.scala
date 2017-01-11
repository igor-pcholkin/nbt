package com.random.nbt

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import java.io.File
import com.typesafe.scalalogging.LazyLogging

object CommandLineExecutor extends LazyLogging {
  def execute(cmdLine: String)(implicit context: Context) = {
    import sys.process._
    val refinedCmdLine = context.resolveVarsIn(cmdLine)
    logger.info(s"Executing command: $refinedCmdLine")
    val workingDir = context.getString("projectDir", "currentDir").getOrElse(".")
    val workingDirAsFile = new File(workingDir)
    Try {
      if (workingDirAsFile.exists())
        Process(refinedCmdLine, workingDirAsFile).!!
      else
        Process(refinedCmdLine).!!
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
