package com.github.giamo.adventofcode18

import scala.io.Source

object Utils {

  def printSolutions(p1: Any, p2: Any): Unit = {
    println(s"Solution for part 1: $p1")
    println(s"Solution for part 2: $p2")
  }

  def getInputLinesAsStream(inputName: String): Seq[String] =
    Source.fromResource(inputName).getLines().toStream
}
