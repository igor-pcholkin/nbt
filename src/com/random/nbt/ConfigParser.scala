package com.random.nbt

import scala.io.Source
import java.io.FileReader
import scala.util.parsing.combinator.JavaTokenParsers

case class Phase(name: String, attributes: List[String])

class ConfigParser extends JavaTokenParsers {

  lazy val eol = sys.props("line.separator")

  override val whiteSpace = """[ \t\n]+""".r

  def str = ("""([^\n\r])+""").r

  def phaseName = ("clean" | "compile" | "package" | "run") <~ ":"

  def description = "description" ~ ":" ~> str

  def dependsOn = "depends on" ~ ":" ~> str

  def commands = "commands" ~ ":" ~> str

  def attribute = description | dependsOn | commands

  def phase = phaseName ~ rep(attribute) ^^ {
    case name ~ attributes =>
      Phase(name, attributes)
  }

  def config = rep1(phase)

  def parse(configFile: String = "conf/default.conf") = {
    parseAll(config, new FileReader(configFile)) match {
      case Success(phases, _) => phases
      case ex @ _             => println(ex); List[Map[String, String]]()
    }
  }
}
