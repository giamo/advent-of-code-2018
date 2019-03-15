package com.github.giamo.adventofcode18.day6

import com.github.giamo.adventofcode18.Utils._
import scala.annotation.tailrec

object Day6 extends App {

  val MaxSumOfManhattanDistances = 10000

  val input = getInputLinesAsStream("day6_input")
  printSolutions(puzzle1(input), puzzle2(input))

  def puzzle1(input: Seq[String]): Int = {
    val coordinates  = parseCoordinates(input)
    val (xMax, yMax) = maxDimensions(coordinates)
    val matrix       = fillInMatrix(xMax, yMax, coordinates)

    findLargestArea(matrix, xMax, yMax)
  }

  def puzzle2(input: Seq[String]): Int = {
    val coordinates                = parseCoordinates(input)
    val (xMax, yMax)               = maxDimensions(coordinates)
    val matrix                     = fillInMatrix(xMax, yMax, coordinates)

    findSizeOfRegionWithinDistance(matrix, xMax, yMax, coordinates, MaxSumOfManhattanDistances)
  }

  private def parseCoordinates(lines: Seq[String]) =
    lines.zipWithIndex.map {
      case (s: String, idx: Int) =>
        val Array(x, y) = s.split(",").map(_.trim)
        Coordinate(idx, x.toInt, y.toInt)
    }

  private def maxDimensions(coordinates: Seq[Coordinate]) = {

    @tailrec
    def maxDimensionsRec(c: Seq[Coordinate], dims: (Int, Int)): (Int, Int) = c match {
      case Seq() => dims
      case Seq(d, tail @ _*) if d.x >= dims._1 && d.y >= dims._2 =>
        maxDimensionsRec(tail, (d.x, d.y))
      case Seq(d, tail @ _*) if d.x >= dims._1 && d.y < dims._2 =>
        maxDimensionsRec(tail, (d.x, dims._2))
      case Seq(d, tail @ _*) if d.x < dims._1 && d.y >= dims._2 =>
        maxDimensionsRec(tail, (dims._1, d.y))
      case Seq(d, tail @ _*) if d.x < dims._1 && d.y < dims._2 =>
        maxDimensionsRec(tail, dims)
    }

    maxDimensionsRec(coordinates, (0, 0))
  }

  private def fillInMatrix(xMax: Int, yMax: Int, coordinates: Seq[Coordinate]) = {
    val matrix = Array.ofDim[Int](xMax, yMax)

    (0 until xMax).foreach { x =>
      (0 until yMax).foreach { y =>
        matrix(x)(y) = coordinateAtLowestDistance(Location(x, y), coordinates) match {
          case Some(c) => c.id
          case None    => -1
        }
      }
    }

    matrix
  }

  private def coordinateAtLowestDistance(location: Location,
                                         coordinates: Seq[Coordinate]): Option[Coordinate] = {
    val distances = coordinates.map(c => (c, manhattanDistance(location, c)))
    distances.sortBy(_._2) match {
      case Seq()                                    => None
      case Seq(t)                                   => Some(t._1)
      case Seq(t1, t2, tail @ _*) if t1._2 == t2._2 => None
      case Seq(t1, t2, tail @ _*)                   => Some(t1._1)
    }

  }

  private def manhattanDistance(location: Location, coordinate: Coordinate) =
    Math.abs(location.x - coordinate.x) + Math.abs(location.y - coordinate.y)

  private def findLargestArea(matrix: Array[Array[Int]], xMax: Int, yMax: Int) = {

    def findAreasRec(locations: Seq[Location],
                     areas: Map[Int, Int],
                     onBorder: Set[Int]): (Map[Int, Int], Set[Int]) =
      locations match {
        case Seq() => (areas, onBorder)
        case Seq(Location(x, y), tail @ _*) if matrix(x)(y) == -1 =>
          findAreasRec(tail, areas, onBorder)
        case Seq(Location(x, y), tail @ _*) if x == 0 || x == xMax || y == 0 || y == yMax =>
          val coordinateId = matrix(x)(y)
          val newArea      = areas.getOrElse(coordinateId, 0) + 1
          findAreasRec(tail, areas + (coordinateId -> newArea), onBorder + coordinateId)
        case Seq(Location(x, y), tail @ _*) =>
          val coordinateId = matrix(x)(y)
          val newArea      = areas.getOrElse(coordinateId, 0) + 1
          findAreasRec(tail, areas + (coordinateId -> newArea), onBorder)
      }

    val allLocations                     = allMatrixLocations(matrix, xMax, yMax)
    val (coordinateAreas, areasOnBorder) = findAreasRec(allLocations, Map(), Set())

    coordinateAreas
      .filter { case (k, v) => !areasOnBorder.contains(k) }
      .values
      .max
  }

  private def findSizeOfRegionWithinDistance(
      matrix: Array[Array[Int]],
      xMax: Int,
      yMax: Int,
      coordinates: Seq[Coordinate],
      maxSumOfManhattanDistances: Int
  ) =
    allMatrixLocations(matrix, xMax, yMax)
      .map(sumOfManhattanDistances(_, coordinates))
      .count(_ < maxSumOfManhattanDistances)

  private def sumOfManhattanDistances(location: Location, coordinates: Seq[Coordinate]) =
    coordinates.map(manhattanDistance(location, _)).sum

  private def allMatrixLocations(matrix: Array[Array[Int]], xMax: Int, yMax: Int) =
    (0 until xMax).flatMap(x => (0 until yMax).map(y => Location(x, y)))
}

case class Coordinate(id: Int, x: Int, y: Int)

case class Location(x: Int, y: Int)
