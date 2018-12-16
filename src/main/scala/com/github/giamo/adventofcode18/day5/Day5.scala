package com.github.giamo.adventofcode18.day5

import scala.annotation.tailrec
import com.github.giamo.adventofcode18.Utils._

object Day5 extends App {

  val input = getInputLinesAsStream("day5_input").head
  printSolutions(part1(input), part2(input))

  def part1(input: String): Int = {
    val polymerAfterReaction = react(input)
    polymerAfterReaction.length
  }

  def part2(input: String): Int = {
    findDistinctUnits(input).map { u =>
      val polymerWithoutUnit = removeUnit(input, u)
      val polymerAfterReaction = react(polymerWithoutUnit)
      polymerAfterReaction.length
    }.min
  }

  private def removeUnit(polymer: String, unit: Char) =
    polymer.replaceAll(unit.toLower.toString, "").replaceAll(unit.toUpper.toString, "")

  private def react(polymer: String): String = {
    @tailrec
    def reactRec(p: String, output: StringBuilder): StringBuilder = p.length match {
      case 0 => output
      case 1 => output.append(p)
      case _ if !doesPairReact(p(0), p(1)) =>
        reactRec(removeFirstNElements(p, 1), output.append(p(0)))
      case _ if doesPairReact(p(0), p(1)) && output.nonEmpty =>
        reactRec(output.last + removeFirstNElements (p, 2), removeLastElement(output))
      case _ if doesPairReact(p(0), p(1)) && output.isEmpty =>
        reactRec(removeFirstNElements(p, 2), output)
    }

    reactRec(polymer, new StringBuilder()).toString
  }

  private def doesPairReact(unit1: Char, unit2: Char): Boolean = {
    unit1.toLower == unit2.toLower && (
      (unit1.isLower && unit2.isUpper) ||
      (unit1.isUpper && unit2.isLower)
    )
  }

  private def findDistinctUnits(chain: Seq[Char]): Seq[Char] = chain.map(_.toLower).distinct

  private def removeLastElement(seq: StringBuilder): StringBuilder = seq.dropRight(1)

  private def removeFirstNElements(s: String, n: Int): String = s.drop(n)
}
