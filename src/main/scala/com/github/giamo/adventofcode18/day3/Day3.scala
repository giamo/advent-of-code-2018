package com.github.giamo.adventofcode18.day3

import com.github.giamo.adventofcode18.Utils._

object Day3 extends App {

  val input = getInputLinesAsStream("day3_input")
  printSolutions(puzzle1(input), puzzle2(input))

  def puzzle1(lines: Seq[String]): Int = {
    val rectangles = lines.map(l => parseRectangle(l))
    val matrix = fillInMatrix(rectangles)

    matrix.map(a => a.count(_ > 1)).sum
  }

  def puzzle2(lines: Seq[String]): Int = {
    val rectangles = lines.map(l => parseRectangle(l))
    val matrix = fillInMatrix(rectangles)

    val ourRectangle = rectangles.find { r =>
      pointsToFill(r).forall { case (x, y) => matrix(x)(y) == 1 }
    }.getOrElse(throw new Exception("no rectangle found"))

    ourRectangle.id
  }

  private def fillInMatrix(rectangles: Seq[Rectangle]): Array[Array[Int]] = {
    val matrix = Array.ofDim[Int](1000, 1000)
    val allPointsToFill = rectangles.flatMap(pointsToFill)

    allPointsToFill.foreach { case (x, y) =>
      matrix(x)(y) += 1
    }

    matrix
  }

  private def parseRectangle(line: String): Rectangle = {
    val pattern = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r
    val pattern(id, l, u, w, h) = line
    Rectangle(id.toInt, l.toInt, u.toInt, w.toInt, h.toInt)
  }

  private def pointsToFill(r: Rectangle): Seq[(Int, Int)] = {
    (r.padLeft until r.padLeft + r.width).flatMap { x =>
      (r.padUp until r.padUp + r.height).map(y => (x, y))
    }
  }

}

case class Rectangle(id: Int, padLeft: Int, padUp: Int, width: Int, height: Int)
