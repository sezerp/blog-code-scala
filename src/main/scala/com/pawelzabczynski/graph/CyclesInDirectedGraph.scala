package com.pawelzabczynski.graph

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{Stack, Set => MSet}


/**
 * Detecting cycles in directed graphs using graph coloring
 * */
object CyclesInDirectedGraph extends App {

  private def hasCycleRec(graph: Map[String, Set[String]]): Boolean = {
    val visiting = MSet.empty[String]
    val visited  = MSet.empty[String]

    def checkCycle(node: String): Boolean = {
      if (visited.contains(node)) {
        false
      } else if (visiting.contains(node)) {
        true
      } else {
        visiting.add(node)
        for (n <- graph(node)) {
          if (checkCycle(n)) {
            return true
          }
        }

        visiting.remove(node)
        visited.add(node)
        false
      }
    }

    for (n <- graph.keys) {
      if (checkCycle(n)) return true
    }

    false
  }

  private def hasCycleStackSafe(graph: Map[String, Set[String]]): Boolean = {
    val visiting = MSet.empty[String]
    val visited  = MSet.empty[String]
    val stack    = mutable.Stack.empty[String]

    for (n <- graph.keys) {
      stack.push(n)
      while (stack.nonEmpty) {
        val cn = stack.pop()
        if (!visited(cn)) {
          val descendants = graph(cn)
          if (descendants.isEmpty || descendants.forall(visited)) {
            visiting.remove(cn)
            visited.add(cn)
          } else if (visiting(cn)) {
            return true
          } else {
            visiting.add(cn)
            for (d <- descendants) {
              stack.push(d)
            }
          }
        }
      }
      visited.addAll(visiting)
      visiting.clear()
    }

    false
  }

  private def hasCycleTailRec(graph: Map[String, Set[String]]): Boolean = {
    val visiting = MSet.empty[String]
    val visited  = MSet.empty[String]

    @tailrec
    def hasCycle(stack: List[String]): Boolean = {
      stack match {
        case Nil =>
          visited.addAll(visiting)
          visiting.clear()
          false
        case head :: tail if visited(head) => hasCycle(tail)
        case head :: tail =>
          graph(head).filterNot(visited).toList match {
            case Nil =>
              visited.add(head)
              visiting.remove(head)
              hasCycle(tail)
            case _ if visiting(head) => true
            case descendants         =>
              visiting.add(head)
              hasCycle(descendants ++ tail)
          }
      }
    }

    @tailrec
    def loop(xs: List[String]): Boolean = {
      xs match {
        case Nil                                   => false
        case head :: tail if !hasCycle(List(head)) => loop(tail)
        case _                                     => true
      }
    }

    loop(graph.keys.toList)
  }

  private def hasCycleTailRecMoreFunctional(graph: Map[String, Set[String]]): Boolean = {
    @tailrec
    def hasCycle(stack: List[String], visiting: Set[String], visited: Set[String]): (Boolean, Set[String]) = {
      stack match {
        case Nil => (false, visited ++ visiting)
        case head :: tail if visited(head) => hasCycle(tail, visiting, visited)
        case head :: tail =>
          graph(head).filterNot(visited).toList match {
            case Nil => hasCycle(tail, visiting - head, visited + head)
            case _ if visiting(head) => (true, visited)
            case descendants         => hasCycle(descendants ++ tail, visiting + head, visited)
          }
      }
    }


    def loop(xs: List[String], visited: Set[String]): Boolean = {
      xs match {
        case Nil => false
        case head :: tail => hasCycle(List(head), Set.empty, visited) match {
          case (isCycle, visited) if !isCycle => loop(tail, visited)
          case _ => true
        }
      }
    }


    loop(graph.keys.toList, Set.empty)
  }




  private val graphWithCycle = Map(
    "a" -> Set("b", "c", "d"),
    "b" -> Set.empty[String],
    "c" -> Set("e"),
    "d" -> Set.empty[String],
    "e" -> Set("f", "b"),
    "f" -> Set("d", "c")
  )

  private val graphWithoutCycle = Map(
    "a" -> Set("b", "c", "d"),
    "b" -> Set.empty[String],
    "c" -> Set("e"),
    "d" -> Set.empty[String],
    "e" -> Set("f", "b"),
    "f" -> Set("d")
  )

  private val largerGraph = Map(
    "a" -> Set("f"),
    "b" -> Set("i", "n", "f"),
    "c" -> Set("k"),
    "d" -> Set("j", "b", "f"),
    "e" -> Set("h", "c", "g", "l", "m"),
    "f" -> Set.empty[String],
    "g" -> Set("n"),
    "h" -> Set.empty[String],
    "i" -> Set("a", "k", "f"),
    "j" -> Set("i", "n", "f"),
    "k" -> Set.empty[String],
    "l" -> Set("b"),
    "m" -> Set("d"),
    "n" -> Set("a", "k", "f")
  )

  private val largerGraphCycle = Map(
    "a" -> Set("f"),
    "b" -> Set("i", "n", "f"),
    "c" -> Set("k"),
    "d" -> Set("j", "b", "f"),
    "e" -> Set("h", "c", "g", "l", "m"),
    "f" -> Set("e"),
    "g" -> Set("n"),
    "h" -> Set.empty[String],
    "i" -> Set("a", "k", "f"),
    "j" -> Set("i", "n", "f"),
    "k" -> Set.empty[String],
    "l" -> Set("b"),
    "m" -> Set("d"),
    "n" -> Set("a", "k", "f")
  )


  println(
    s"""
       |---------------------------------
       | hasCycleRec
       |---------------------------------
       |""".stripMargin)

  println(hasCycleRec(graphWithCycle))
  println(hasCycleRec(graphWithoutCycle))
  println(hasCycleRec(largerGraph))
  println(hasCycleRec(largerGraphCycle))

  println(
    s"""
       |---------------------------------
       | hasCycleStackSafe
       |---------------------------------
       |""".stripMargin)

  println(hasCycleStackSafe(graphWithCycle))
  println(hasCycleStackSafe(graphWithoutCycle))
  println(hasCycleStackSafe(largerGraph))
  println(hasCycleStackSafe(largerGraphCycle))

  println(
    s"""
       |---------------------------------
       | hasCycleTailRec
       |---------------------------------
       |""".stripMargin)

  println(hasCycleTailRec(graphWithCycle))
  println(hasCycleTailRec(graphWithoutCycle))
  println(hasCycleTailRec(largerGraph))
  println(hasCycleTailRec(largerGraphCycle))

  println(
    s"""
       |---------------------------------
       | hasCycleTailRecMoreFunctional
       |---------------------------------
       |""".stripMargin)

  println(hasCycleTailRecMoreFunctional(graphWithCycle))
  println(hasCycleTailRecMoreFunctional(graphWithoutCycle))
  println(hasCycleTailRecMoreFunctional(largerGraph))
  println(hasCycleTailRecMoreFunctional(largerGraphCycle))

}
