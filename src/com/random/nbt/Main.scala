package com.random.nbt

object Main extends App {
  if (args.length > 0) {
    implicit val phases = new ConfigParser().parse()
    val phase2RunStr = args(0)
    phases.find(_.name == phase2RunStr) match {
      case Some(phase) => execute(phase)
      case None => // nothing to do
    }
  }

  def executeCommmandsOfPhase(phase: Phase) = {
    phase.cmdLines map { cmd =>
      executeCmdLine(cmd)
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
    executeCommmandsOfPhase(phase)
  }

  def executeCmdLine(cmdLine: String) = {
    import sys.process._
    cmdLine.!
  }
}
