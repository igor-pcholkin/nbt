package com.random.nbt

import java.io.File

object Main extends App {
  if (args.length > 0) {
    implicit val phases = new ConfigParser().parse()
    val phaseExecutor = new PhaseExecutor()
    phaseExecutor.runPhase("init")
    phaseExecutor.runPhase(args(0))
  } else {
    println("Usage: nbt <build phase>")
  }
}
