package com.github.giamo.adventofcode18.day4

import scala.annotation.tailrec
import com.github.giamo.adventofcode18.Utils._

object Day4 extends App {

  val input = getInputLinesAsStream("day4_input")
  printSolutions(part1(input), part2(input))

  def part1(lines: Seq[String]): Long = {
    val recordEntries = lines.map(parseLogRecord)
    val sortedEntries = recordEntries.sortBy(e => s"${e.date} ${e.hour}:${e.minute}")
    val guardsStats = findGuardsStats(sortedEntries)

    val sleepiestGuardStats = guardsStats.maxBy(_._2.totalMinutesAsleep)._2
    val mostAsleepMinute = sleepiestGuardStats.minutesMap.maxBy(_._2)._1

    sleepiestGuardStats.guardId * mostAsleepMinute
  }

  def part2(lines: Seq[String]): Long = {
    val recordEntries = lines.map(parseLogRecord)
    val sortedEntries = recordEntries.sortBy(e => s"${e.date} ${e.hour}:${e.minute}")
    val guardsStats = findGuardsStats(sortedEntries)

    val guardsByMaxAsleepSameMinute: Map[Long, (Int, Int)] = guardsStats.map { case (k, v) =>
      k -> v.minutesMap.maxBy(_._2)
    }

    val guardMostAsleepSameMinute = guardsByMaxAsleepSameMinute.maxBy(_._2._2)
    guardMostAsleepSameMinute._1 * guardMostAsleepSameMinute._2._1
  }

  private def findGuardsStats(recordEntries: Seq[LogRecord]): Map[Long, GuardStats] = {

    @tailrec
    def updateMinutesMapRec(minutes: Seq[Int], map: Map[Int, Int]): Map[Int, Int] = minutes match {
      case Seq() => map
      case Seq(m, tail @ _*) =>
        val newValue = map.getOrElse(m, 0) + 1
        updateMinutesMapRec(tail, map + (m -> newValue))
    }

    @tailrec
    def findGuardsStatsRec(entries: Seq[LogRecord], guardStatsMap: Map[Long, GuardStats], currentGuard: Long, lastFellAsleepMinute: String): Map[Long, GuardStats] = entries match {
      case Seq() => guardStatsMap

      case Seq(e: BeginsShiftRecord, tail @ _*) =>
        findGuardsStatsRec(tail, guardStatsMap, e.guardId, lastFellAsleepMinute)

      case Seq(e: FallsAsleepRecord, tail @ _*) =>
        findGuardsStatsRec(tail, guardStatsMap, currentGuard, e.minute)

      case Seq(e: WakesUpRecord, tail @ _*) =>
        val numMinutesAsleep = e.minute.toInt - lastFellAsleepMinute.toInt
        val seqMinutesAsleep = minutesRangeToSequence(lastFellAsleepMinute.toInt, e.minute.toInt)

        val updatedGuardStats = guardStatsMap.get(currentGuard) match {
          case Some(g) => g.copy(
            totalMinutesAsleep = g.totalMinutesAsleep + numMinutesAsleep,
            minutesMap = updateMinutesMapRec(seqMinutesAsleep, g.minutesMap)
          )
          case None =>
            GuardStats(currentGuard, numMinutesAsleep, updateMinutesMapRec(seqMinutesAsleep, Map()))
        }

        val updatedMap = guardStatsMap + (currentGuard -> updatedGuardStats)
        findGuardsStatsRec(tail, updatedMap, currentGuard, e.minute)

      case Seq(e, _ @ _*) =>
        throw new Exception(s"unhandled LogRecord event: ${e.getClass}")
    }

    findGuardsStatsRec(recordEntries, Map[Long, GuardStats](), -1, "")
  }

  private def parseLogRecord(line: String) = {
    val recordRegex = "\\[(\\d{4}\\-\\d{2}\\-\\d{2}) (\\d{2}):(\\d{2})\\] (.*)".r
    val recordRegex(d, h, m, e) = line
    e match {
      case "falls asleep" => FallsAsleepRecord(d, h, m)
      case "wakes up" => WakesUpRecord(d, h, m)
      case other =>
        val beginsShiftRegex = "Guard #(\\d+) begins shift".r
        val beginsShiftRegex(guardId) = other
        BeginsShiftRecord(d, h, m, guardId.toLong)
    }
  }

  private def minutesRangeToSequence(start: Int, end: Int): List[Int] = (start until end).toList
}

trait LogRecord {
  val date: String
  val hour: String
  val minute: String
}

case class BeginsShiftRecord(date: String, hour: String, minute: String, guardId: Long) extends LogRecord

case class FallsAsleepRecord(date: String, hour: String, minute: String) extends LogRecord

case class WakesUpRecord(date: String, hour: String, minute: String) extends LogRecord

case class GuardStats(guardId: Long, totalMinutesAsleep: Long, minutesMap: Map[Int, Int])