package com.random.nbt

class PhaseExecutor {
  def runPhase(phaseName: String)(implicit phases: List[Phase], context: Map[String, String]) = {
    phases.find(_.name == phaseName) match {
      case Some(phase) => execute(phase)
      case None => println(s"Error: No such phase found: ${phaseName}")
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
    executeInternalCalls(phase)
    executeCommmandsOfPhase(phase)
  }

  def executeInternalCalls(phase: Phase)(implicit context: Map[String, String]) = {
    if (phase.calls.nonEmpty) {
      val callLine = resolveVarsInCommmandLine(phase.calls.head)
      if (phase.name == "compile") {
        val callParams = callLine.split("[ \t]+")
        if (callParams(0) == "compile") {
          val sourceDir = resolveVarsInCommmandLine(callParams(1))
          println("Compile source dir: " + sourceDir)
          new ScalaCompiler().compile(sourceDir)
        }
      } else if (phase.name == "find") {
        val callParams = callLine.split("[ \t]+")
        if (callParams(0) == "findMainClass") {
          val binDir = callParams(1)
          println("Finding in bin dir: " + binDir)
          println(new ScalaAppRunner().findMainClass(binDir).getOrElse(""))
        }
      }
    }
  }

  def executeCommmandsOfPhase(phase: Phase)(implicit context: Map[String, String]) = {
    phase.cmdLines map { cmd =>
      executeCmdLine(cmd)
    }
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
