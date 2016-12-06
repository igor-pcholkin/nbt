package com.random.nbt

import java.io.File
import scala.util.Try
import scala.util.Failure
import scala.util.Success

class PhaseExecutor extends FileUtils {
  def runPhase(phaseName: String)(implicit phases: List[Phase]) = {
    phases.find(_.name == phaseName) match {
      case Some(phase) => execute(phase)
      case None => println(s"Error: No such phase found: ${phaseName}")
    }
  }

  def executeDependantPhases(phase: Phase)(implicit phases: List[Phase]) = {
    println(s"Executing dependency phases: ${phase.dependsOn.mkString(",")} on phase ${phase.name}")
    phase.dependsOn map { dependsOnPhase =>
      phases.find(_.name == dependsOnPhase) match {
        case Some(resolvedPhase) => execute(resolvedPhase)
        case None => println(s"Error: No dependant phase found: ${dependsOnPhase}")
      }
    }
  }

  def execute(phase: Phase)(implicit phases: List[Phase]): Unit = {
    executeDependantPhases(phase)
    executeInternalCalls(phase)
    executeCommmandsOfPhase(phase)
  }

  def executeInternalCalls(phase: Phase) = {
    if (phase.calls.nonEmpty) {
      phase.calls map { call =>
        val callLine = resolveVarsIn(call)
        if (callLine.contains("=")) {
          new InternalCallHandler("=", Array(callLine)).setVar()
        } else {
          val callParams = callLine.split("[ \t]+")
          new InternalCallHandler(callParams(0), callParams.slice(1, callParams.length)).handle()
        }
      }
    }
  }

  def executeCommmandsOfPhase(phase: Phase) = {
    phase.cmdLines map { cmd =>
      executeCmdLine(cmd)
    }
  }

  def executeCmdLine(cmdLine: String) = {
    import sys.process._
    val refinedCmdLine = resolveVarsIn(cmdLine)
    println(s"Executing command: $refinedCmdLine")
    val workingDir = Context.getString("projectDir", "currentDir").getOrElse(".")
    println(s"Running in working dir: $workingDir")
    Try {
      Process(refinedCmdLine, new java.io.File(workingDir)).!!
    } match {
      case Success(output: String) if output.nonEmpty => println(output)
      case Failure(ex) => println(ex.getMessage)
      case _ =>
    }
  }

  def resolveVarsIn(line: String) = {
    if (line.contains("$")) {
      Context.getKeys.foldLeft(line) { (cmdLine, key) =>
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
