package com.random.nbt

import java.io.File
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import com.typesafe.scalalogging.LazyLogging

object PhaseExecutor extends FileUtils with LazyLogging {
  def runPhase(phaseName: String)(implicit phases: List[Phase]) = {
    phases.find(_.name == phaseName) match {
      case Some(phase) => execute(phase)
      case None => logger.error(s"No such phase found: ${phaseName}")
    }
  }

  def executeDependantPhases(phase: Phase)(implicit phases: List[Phase]): Boolean = {
    logger.info(s"Executing dependency phases: ${phase.dependsOn.mkString(",")} on phase ${phase.name}")
    val executedDependencyPhases = phase.dependsOn takeWhile { dependsOnPhase =>
      phases.find(_.name == dependsOnPhase) match {
        case Some(resolvedPhase) => execute(resolvedPhase)
        case None =>
          logger.error(s"No depending phase found: ${dependsOnPhase}")
          false
      }
    }
    executedDependencyPhases.length == phase.dependsOn.length
  }

  def execute(phase: Phase)(implicit phases: List[Phase]): Boolean = {
    executeDependantPhases(phase) &&
    executeSets(phase) &&
    executeInternalCalls(phase) &&
    executeCommmandsOfPhase(phase)
  }

  def executeSets(phase: Phase) = {
    val executedSets = phase.sets.takeWhile { set =>
      val setLine = resolveVarsIn(set)
      if (setLine.contains("=")) {
        new InternalCallHandler("=", Array(setLine)).setVar()
      } else false
    }
    executedSets.length == phase.sets.length
  }

  def executeInternalCalls(phase: Phase) = {
    val executedCalls = phase.calls.takeWhile { call =>
      val callLine = resolveVarsIn(call)
      val callParams = callLine.split("[ \t]+")
      new InternalCallHandler(callParams(0), callParams.slice(1, callParams.length)).handle()
    }
    executedCalls.length == phase.calls.length
  }

  def executeCommmandsOfPhase(phase: Phase) = {
    val executedCommands = phase.cmdLines takeWhile { cmd =>
      executeCmdLine(cmd)
    }
    executedCommands.length == phase.cmdLines.length
  }

  def executeCmdLine(cmdLine: String) = {
    import sys.process._
    val refinedCmdLine = resolveVarsIn(cmdLine)
    logger.info(s"Executing command: $refinedCmdLine")
    val workingDir = InternalCallHandler.getProjectDir.getOrElse(".")
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

  def resolveVarsIn(line: String) = {
    if (line.contains("$")) {
      Context.getKeys.toSeq.sortBy(-_.length).foldLeft(line) { (cmdLine, key) =>
        val value = Context.get(key) match {
          case Some(value: String) => value
          case _ => ""
        }
        cmdLine.replace("$" + s"$key", value)
      }
    } else {
      line
    }
  }

}
