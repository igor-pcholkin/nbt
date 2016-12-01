package com.random.nbt

import scala.io.Source
import java.io.FileReader
import scala.util.parsing.combinator.JavaTokenParsers

class Attribute
case class Phase(name: String, attributes: List[Attribute])
case class Command(cmdLine: String) extends Attribute
case class Description(value: String) extends Attribute
case class Depends(phases: List[Phase]) extends Attribute

case class Commands(commands: List[Command]) extends Attribute

class ConfigParser extends JavaTokenParsers {

  lazy val eol = sys.props("line.separator")

  override val whiteSpace = """[ \t\n]+""".r

  def str = ("""([^\n\r])+""").r ^^ { s => s.trim()
  }

  def phaseName = ("clean" | "compile" | "package" | "run") <~ ":"

  def description = "description" ~ ":" ~> str ^^ { value =>
    Description(value)
  }

  def dependsOn = "depends on" ~ ":" ~> rep1(str, ",") ^^ { values =>
    Depends(values.map(Phase(_, Nil)))
  }

  def commands = "commands" ~ ":" ~> rep1(str, eol) ^^ { cmdLines =>
    Commands(cmdLines.map(Command(_)))
  }

  def attribute = description | dependsOn | commands

  def phase = phaseName ~ rep(attribute) ^^ {
    case name ~ attributes =>
      Phase(name, attributes)
  }

  def config = rep1(phase)

  def parse(configFile: String = "conf/default.conf") = {
    parseAll(config, new FileReader(configFile)) match {
      case Success(phases, _) => phases
      case ex @ _             => println(ex); List[Phase]()
    }
  }
}
