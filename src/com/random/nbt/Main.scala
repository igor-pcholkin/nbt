package com.random.nbt

object Main extends App {
  if (args.length > 0) {
    val phases = new ConfigParser().parse()
    val phase2RunStr = args(0)
    phases.find(_.name == phase2RunStr) match {
      case Some(phase) => execute(phase)
      case None => // nothing to do
    }
  }

  def execute(phase: Phase) = {
    val phaseCommands = phase.attributes.collect { case c: Commands => c }
    phaseCommands.head.commands.map { cmd =>
      executeCmdLine(cmd.cmdLine)
    }
  }

  def executeCmdLine(cmdLine: String) = {
    import sys.process._
    cmdLine.!
  }
}
