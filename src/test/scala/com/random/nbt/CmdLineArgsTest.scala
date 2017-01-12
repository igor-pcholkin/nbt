package com.random.nbt

import java.io.File

import org.scalatest._

class CmdLineArgsTest extends FlatSpec with MustMatchers with BeforeAndAfter {

  "Main" should "parse command line args" in {
    Main.parseArgs(Array("run", "-vars", "workDir=pp", "dependencies=scalaz, org.logback:logback")) mustBe
      ("run", Map("workDir" -> "pp", "dependencies" -> "scalaz, org.logback:logback"))
    Main.parseArgs(Array("-vars", "workDir=pp", "run")) mustBe ("run", Map("workDir" -> "pp"))
  }

}
