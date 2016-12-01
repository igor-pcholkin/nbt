package com.random.nbt

import java.io.File

object Main extends App {
  if (args.length > 0) {
    implicit val phases = new ConfigParser().parse()
    implicit val context = createContext
    runPhase(args(0))
  }

  def createContext = {
    val currentDir = new File(".").getCanonicalPath
    val projectName = currentDir.substring(currentDir.lastIndexOf(File.separator) + 1)
    Map[String, String](
        "currentDir" -> currentDir,
        "projectName" -> projectName
        )
  }

  def runPhase(phaseName: String)(implicit phases: List[Phase], context: Map[String, String]) = {
    phases.find(_.name == phaseName) match {
      case Some(phase) => execute(phase)
      case None => println(s"Error: No such phase found: ${phaseName}")
    }
  }

  def executeCommmandsOfPhase(phase: Phase)(implicit context: Map[String, String]) = {
    phase.cmdLines map { cmd =>
      executeCmdLine(cmd)
    }
  }

  def executeDependantPhases(phase: Phase)(implicit phases: List[Phase], context: Map[String, String]) = {
    phase.dependsOn map { dependantPhase =>
      phases.find(_.name == dependantPhase.name) match {
        case Some(resolvedPhase) => execute(resolvedPhase)
        case None => println(s"Error: No dependant phase found: ${dependantPhase.name}")
      }
    }
  }

  def execute(phase: Phase)(implicit phases: List[Phase], context: Map[String, String]): Unit = {
    executeDependantPhases(phase)
    executeCommmandsOfPhase(phase)
  }

  def executeCmdLine(cmdLine: String)(implicit context: Map[String, String]) = {
    import sys.process._
    val refinedCmdLine = resolveVarsInCommmandLine(cmdLine)
    refinedCmdLine.!
  }

  def resolveVarsInCommmandLine(cmdLine: String)(implicit context: Map[String, String]) = {
    context.keys.foldLeft(cmdLine) { (cmdLine, key) =>
      cmdLine.replace("$" + s"$key", s"${context.getOrElse(key, "")}")
    }
  }
}
