package com.random.nbt

import scala.collection.mutable.Map

class PhaseExecutor(implicit val context: Map[String, Any]) {
  def runPhase(phaseName: String)(implicit phases: List[Phase]) = {
    phases.find(_.name == phaseName) match {
      case Some(phase) => execute(phase)
      case None => println(s"Error: No such phase found: ${phaseName}")
    }
  }

  def executeDependantPhases(phase: Phase)(implicit phases: List[Phase]) = {
    phase.dependsOn map { dependantPhase =>
      phases.find(_.name == dependantPhase.name) match {
        case Some(resolvedPhase) => execute(resolvedPhase)
        case None => println(s"Error: No dependant phase found: ${dependantPhase.name}")
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
      val callLine = resolveVarsInCommmandLine(phase.calls.head)
      val callParams = callLine.split("[ \t]+")
      new InternalCallHandler(callParams(0), callParams.slice(1, callParams.length)).handle()
    }
  }

  def executeCommmandsOfPhase(phase: Phase) = {
    phase.cmdLines map { cmd =>
      executeCmdLine(cmd)
    }
  }

  def executeCmdLine(cmdLine: String) = {
    import sys.process._
    val refinedCmdLine = resolveVarsInCommmandLine(cmdLine)
    println(s"Executing command: $refinedCmdLine")
    refinedCmdLine.!
  }

  def resolveVarsInCommmandLine(cmdLine: String) = {
    context.keys.foldLeft(cmdLine) { (cmdLine, key) =>
      cmdLine.replace("$" + s"$key", s"${context.getOrElse(key, "")}")
    }
  }

}
