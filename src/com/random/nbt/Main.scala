package com.random.nbt

import java.io.File

object Main extends App {
  implicit val context = new ContextCreator().create
  if (args.length > 0) {
    implicit val phases = new ConfigParser().parse()
    new PhaseExecutor().runPhase(args(0))
  }
}
