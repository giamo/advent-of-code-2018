package com.github.giamo.adventofcode18.day7

import com.github.giamo.adventofcode18.Utils._

import scala.annotation.tailrec
import scala.collection.mutable

object Day7 extends App {

  val NUM_WORKERS = 5
  val BASE_STEP_DELAY = 60
  val ordering: Ordering[Char] = Ordering[Char].reverse // alphabetical order

  val input = getInputLinesAsStream("day7_input")
  printSolutions(part1(input), part2(input))

  def part1(input: Seq[String]): String = {
    val deps = parseInstructions(input)
    findOutputSequence(deps)
  }

  def part2(input: Seq[String]): Int = {
    val deps = parseInstructions(input)
    computeTotalDelay(deps, NUM_WORKERS)
  }

  private def findOutputSequence(initialDeps: Map[Char, Set[Char]]) = {
    val queue = mutable.PriorityQueue[Char]()(ordering)

    @tailrec
    def findOrderedSequenceRec(deps: Map[Char, Set[Char]], output: String): String = deps match {
      case m if m.isEmpty => output
      case m =>
        val next = queue.dequeue()
        val newDeps = updatedDependencies(m, next)
        findStepsWithoutDependencies(newDeps).foreach { s =>
          if (!queue.exists(_ == s)) queue.enqueue(s)
        }
        findOrderedSequenceRec(newDeps, output + next)
    }

    queue.enqueue(findStepsWithoutDependencies(initialDeps).toSeq: _*)
    findOrderedSequenceRec(initialDeps, "")
  }

  private def computeTotalDelay(initalDeps: Map[Char, Set[Char]], numWorkers: Int) = {
    val stepsReadyQueue = mutable.PriorityQueue[Char]()(ordering)

    def assignStepToWorker(workerId: Int) = if (stepsReadyQueue.nonEmpty) {
      val step = stepsReadyQueue.dequeue()
      Worker(workerId, Some(step, stepDelay(step)))
    } else Worker(workerId, None)

    def areAllWorkersIdle(workers: Seq[Worker]) = workers.forall(_.busyFor.isEmpty)

    def isStepAssigned(stepId: Char, workers: Seq[Worker]) =
      workers.exists(w => w.busyFor.isDefined && w.busyFor.get._1 == stepId)

    def updateStepsReadyToStart(deps: Map[Char, Set[Char]], workers: Seq[Worker]) = {
      val stepsReadyToStart = findStepsWithoutDependencies(deps).toSeq
      stepsReadyToStart.foreach { s =>
        if (!isStepAssigned(s, workers) && !stepsReadyQueue.exists(_ == s))
          stepsReadyQueue.enqueue(s)
      }
    }

    @tailrec
    def computeTotalDelayRec(workers: Seq[Worker], deps: Map[Char, Set[Char]], completed: Set[Char], totalDelay: Int): Int = {
      updateStepsReadyToStart(deps, workers)

      var newlyCompleted: Set[Char] = Set.empty
      var updatedDeps = deps

      val updatedWorkers = workers.sortBy(w => w.busyFor.map(_._2).getOrElse(Int.MaxValue)).map {
        case Worker(id, None) =>
          assignStepToWorker(id)
        case Worker(id, Some((c, 1))) =>
          newlyCompleted += c
          updatedDeps = updatedDependencies(updatedDeps, c)
          updateStepsReadyToStart(updatedDeps, workers)
          assignStepToWorker(id)
          //Worker(id, None)
        case Worker(id, Some((c, 0))) =>
          newlyCompleted += c
          assignStepToWorker(id)
        case Worker(id, Some((c, n))) =>
          Worker(id, Some((c, n-1)))
      }

      if (updatedDeps.isEmpty && areAllWorkersIdle(updatedWorkers)) {
        totalDelay
      } else {
        val updatedCompleted = completed ++ newlyCompleted
        computeTotalDelayRec(updatedWorkers, updatedDeps, updatedCompleted, totalDelay + 1)
      }
    }

    val initialWorkers = (0 until numWorkers).map { i => Worker(i, None) }
    computeTotalDelayRec(initialWorkers, initalDeps, Set(), 0)

  }

  private def parseInstructions(lines: Seq[String]) = {
    val Pattern = "Step ([a-zA-Z]{1}) must be finished before step (.*) can begin.".r

    @tailrec
    def parseInstructionsRec(list: Seq[String], deps: Map[Char, Set[Char]]): Map[Char, Set[Char]] = list match {
      case Seq() => deps
      case Seq(l, tail @ _*) =>
        val Pattern(t, f) = l
        val (to, from) = (t.charAt(0), f.charAt(0))
        val newDepValueFrom = deps.getOrElse(from, Set()) + to
        val newDepValueTo = deps.getOrElse(to, Set())
        parseInstructionsRec(tail, deps + (from -> newDepValueFrom) + (to -> newDepValueTo))
    }

    parseInstructionsRec(lines, Map())
  }

  private def findStepsWithoutDependencies(deps: Map[Char, Set[Char]]) =
    deps.filter { case (_,v) => v.isEmpty }.keySet

  private def updatedDependencies(deps: Map[Char, Set[Char]], completed: Char) =
    deps.map { case (k, v) => (k, v - completed)} - completed

  private def updatedDependenciesMulti(deps: Map[Char, Set[Char]], completed: Set[Char]) =
    completed.foldLeft(deps) { (incr, c) => updatedDependencies(incr, c) }

  private def stepDelay(id: Char) = BASE_STEP_DELAY + charToNumber(id)

  private def charToNumber(char: Char) = char.toUpper.toInt - 64
}

case class Worker(id: Int, busyFor: Option[(Char, Int)])