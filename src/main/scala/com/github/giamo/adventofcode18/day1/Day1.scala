package com.github.giamo.adventofcode18.day1

import scala.annotation.tailrec
import com.github.giamo.adventofcode18.Utils._

object Day1 extends App {

  val input = getInputLinesAsStream("day1_input")
  printSolutions(puzzle1(input), puzzle2(input))

  def puzzle1(lines: Seq[String]): Long = {
    @tailrec
    def solveRec(remaining: Seq[String], incr: Long): Long = remaining match {
      case Seq(v, tail @ _*) => solveRec(tail, incr + v.toLong)
      case Seq() => incr
    }

    solveRec(lines, 0)
  }

  def puzzle2(lines: Seq[String]): Long = {
    @tailrec
    def solveRec(remaining: Seq[String], incr: Long, seen: Set[Long]): Long = remaining match {
      case Seq(v, tail @ _*) =>
        val newFrequency = incr + v.toLong
        if (seen.contains(newFrequency)) newFrequency
        else solveRec(tail, newFrequency, seen + newFrequency)
      case Seq() => solveRec(lines, incr, seen)
    }

    solveRec(lines, 0L, Set(0L))
  }

}