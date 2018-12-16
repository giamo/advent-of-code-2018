package com.github.giamo.adventofcode18.day2

import scala.annotation.tailrec
import com.github.giamo.adventofcode18.Utils._

object Day2 extends App {

  val input = getInputLinesAsStream("day2_input")
  printSolutions(part1(input), part2(input))

  def part1(ids: Seq[String]): Int = {
    val (occurrences2, occurrences3) = ids
      .map(twoAndThreeCharsOccurence)
      .foldLeft((0, 0)) {
        case ((occ2, occ3), (true, true)) => (occ2+1, occ3+1)
        case ((occ2, occ3), (true, false)) => (occ2+1, occ3)
        case ((occ2, occ3), (false, true)) => (occ2, occ3+1)
        case ((occ2, occ3), (false, false)) => (occ2, occ3)
      }

    occurrences2 * occurrences3
  }

  def part2(ids: Seq[String]): String = {
    findRightBoxes(ids) match {
      case Seq(box1, box2) => commonLetters(box1, box2)
      case Seq() => throw new Exception("no solution")
      case s => throw new Exception(s"unexpected result sequence $s")
    }
  }

  private def twoAndThreeCharsOccurence(id: String): (Boolean, Boolean) = {
    val charsCountMap: Map[Char, Int] = countCharsOccurrence(id)

    charsCountMap.foldLeft((false, false)) {
      case ((b1, b2), (_, 2)) => (true, b2)
      case ((b1, b2), (_, 3)) => (b1, true)
      case ((b1, b2), (_, _)) => (b1, b2)
    }
  }

  private def countCharsOccurrence(id: String): Map[Char, Int] = {
    @tailrec
    def countLettersRec(s: String, map: Map[Char, Int]): Map[Char, Int] = s.length match {
      case 0 => map
      case _ =>
        val c = s.charAt(0)
        val newValue = map.getOrElse(c, 0) + 1
        countLettersRec(s.tail, map ++ Map(c -> newValue))
    }

    countLettersRec(id, Map())
  }

  private def findRightBoxes(ids: Seq[String]): Seq[String] = {
    val remaining = ids.combinations(2).dropWhile { case Seq(id1, id2) =>
      commonLetters(id1, id2) match {
        case s if s.length == id1.length-1 && s.length == id2.length-1 => false
        case _ => true
      }
    }

    if (remaining.isEmpty) Seq()
    else remaining.next()
  }

  private def commonLetters(id1: String, id2: String): String = {
    @tailrec
    def commonLettersRec(i1: Seq[Char], i2: Seq[Char], common: String): String = (i1, i2) match {
      case (_, Seq()) | (Seq(), _) =>
        common
      case (Seq(c1, tail1 @ _*), Seq(c2, tail2 @ _*)) if c1 == c2 =>
        commonLettersRec(tail1, tail2, common + c1)
      case (Seq(_, tail1 @ _*), Seq(_, tail2 @ _*)) =>
        commonLettersRec(tail1, tail2, common)
    }

    commonLettersRec(id1.toSeq, id2.toSeq, "")
  }
}
