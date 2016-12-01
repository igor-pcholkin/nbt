package com.random.nbt

import scala.io.Source
import java.io.FileReader
import scala.util.parsing.combinator.JavaTokenParsers

case class Phase(name: String, cmdLines: List[String], description: Option[String], dependsOn: List[DependantPhase])
case class DependantPhase(name: String)

class Attribute

case class Command(cmdLine: String) extends Attribute
case class Description(value: String) extends Attribute
case class Depends(phases: List[DependantPhase]) extends Attribute

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
    Depends(values.map(DependantPhase(_)))
  }

  def commands = "commands" ~ ":" ~> rep1(str, eol) ^^ { cmdLines =>
    Commands(cmdLines.map(Command(_)))
  }

  def attribute = description | dependsOn | commands

  def phase = phaseName ~ rep(attribute) ^^ {
    case name ~ attributes =>
      val cmdLines = (attributes collect { case cmds: Commands => cmds.commands }).headOption.getOrElse(Nil) map { cmd => cmd.cmdLine }
      val description = (attributes collect { case d: Description => d.value }).headOption
      val dependsOn = (attributes collect { case d: Depends => d.phases }).headOption.getOrElse(Nil)
      Phase(name, cmdLines, description, dependsOn)
  }

  def config = rep1(phase)

  def parse(configFile: String = "conf/default.conf") = {
    parseAll(config, new FileReader(configFile)) match {
      case Success(phases, _) => phases
      case ex @ _             => println(ex); List[Phase]()
    }
  }
}
