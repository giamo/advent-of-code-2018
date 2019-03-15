package com.github.giamo.adventofcode18.day8

import com.github.giamo.adventofcode18.Utils._

object Day8 extends App {

  val input = getInputLinesAsStream("day8_input").head.split(" ").map(_.toInt)
  printSolutions(part1(input), part2(input))

  def part1(input: Seq[Int]): Int = parse(input)._1

  def part2(input: Seq[Int]): Int = parse(input)._3

  private def parse(data: Seq[Int]): (Int, Seq[Int], Int) = {

    def recurseChildren(payload: Seq[Int],
                        numChildren: Int,
                        subtotals: Seq[Int],
                        subvalues: Seq[Int]): (Seq[Int], Int, Seq[Int]) =
      if (numChildren == 0) (payload, subtotals.sum, subvalues)
      else {
        val (subtotal, subpayload, subvalue) = parse(payload)
        recurseChildren(subpayload, numChildren - 1, subtotals :+ subtotal, subvalues :+ subvalue)
      }

    val numChildren = data.head
    val numMetas    = data(1)
    val payload     = data.drop(2)

    val (subpayload, subtotal, subvalues) = recurseChildren(payload, numChildren, Seq(), Seq())

    val metadatas = subpayload.take(numMetas)
    val nodeValue = (for {
      i <- metadatas.indices
      m = metadatas(i) if m > 0 && m <= subvalues.length
      s = subvalues(m - 1)
    } yield s).sum

    if (numChildren == 0) (subtotal + metadatas.sum, subpayload.drop(numMetas), metadatas.sum)
    else (subtotal + metadatas.sum, subpayload.drop(numMetas), nodeValue)
  }
}
