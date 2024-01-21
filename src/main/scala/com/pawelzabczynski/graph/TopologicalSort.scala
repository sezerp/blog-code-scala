package com.pawelzabczynski.graph

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{Set => MSet, Stack => MStack}

object TopologicalSort extends App {

  private def sortGraphRecursive(
      graph: Map[String, List[String]]
  ): List[String] = {
    val visiting = MSet.empty[String]
    val visited  = MSet.empty[String]
    val result   = MStack.empty[String]

    def checkCycle(node: String): Boolean = {
      if (visited(node)) {
        false
      } else if (visiting(node)) {
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
        result.push(node)
        false
      }
    }

    for (n <- graph.keys) {
      if (checkCycle(n)) {
        return Nil
      }
    }

    result.toList
  }

  private def sortIterative(graph: Map[String, List[String]]): List[String] = {
    val visiting = MSet.empty[String]
    val visited  = MSet.empty[String]
    val stack    = mutable.Stack.empty[String]
    val result   = MStack.empty[String]

    for (n <- graph.keys) {
      stack.push(n)
      while (stack.nonEmpty) {
        val cn = stack.pop()
        if (!visited(cn)) {
          val descendants = graph(cn)
          if (descendants.isEmpty || descendants.forall(visited)) {
            visiting.remove(cn)
            visited.add(cn)
            result.push(cn)
          } else if (visiting(cn)) {
            return Nil
          } else {
            visiting.add(cn)
            for (d <- descendants) {
              stack.push(d)
            }
          }
        }
      }
      visited.addAll(visiting)
      visiting.foreach(result.push)
      visiting.clear()
    }

    result.toList
  }

  private def sortTailRec(graph: Map[String, List[String]]): List[String] = {
    val visiting = MSet.empty[String]
    val visited  = MSet.empty[String]
    val result   = MStack.empty[String]

    @tailrec
    def hasCycle(stack: List[String]): Boolean = {
      stack match {
        case Nil =>
          visited.addAll(visiting)
          visiting.foreach(result.push)
          visiting.clear()
          false
        case head :: tail if visited(head) => hasCycle(tail)
        case head :: tail =>
          graph(head).filterNot(visited) match {
            case Nil =>
              visited.add(head)
              visiting.remove(head)
              result.push(head)
              hasCycle(tail)
            case _ if visiting(head) => true
            case descendants =>
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

    if (loop(graph.keys.toList)) Nil else result.toList
  }

  private def sortTailRecMoreFunctional(
      graph: Map[String, List[String]]
  ): List[String] = {
    @tailrec
    def hasCycle(
        stack: List[String],
        visiting: Set[String],
        visited: Set[String],
        acc: List[String]
    ): (Boolean, Set[String], List[String]) = {
      stack match {
        case Nil => (false, visited ++ visiting, visiting.toList ++ acc)
        case head :: tail if visited(head) =>
          hasCycle(tail, visiting, visited, acc)
        case head :: tail =>
          graph(head).filterNot(visited) match {
            case Nil =>
              hasCycle(tail, visiting - head, visited + head, head :: acc)
            case _ if visiting(head) => (true, visited, acc)
            case descendants =>
              hasCycle(descendants ++ tail, visiting + head, visited, acc)
          }
      }
    }

    @tailrec
    def loop(
        xs: List[String],
        visited: Set[String],
        acc: List[String]
    ): List[String] = {
      xs match {
        case Nil => acc
        case head :: tail =>
          hasCycle(List(head), Set.empty, visited, acc) match {
            case (isCycle, visited, result) if !isCycle =>
              loop(tail, visited, result)
            case _ => Nil
          }
      }
    }

    loop(graph.keys.toList, Set.empty, List.empty)
  }

  val digraph = Map(
    "a" -> List("b", "c", "d"),
    "b" -> List("e"),
    "c" -> List("f"),
    "d" -> List("g"),
    "e" -> List("g"),
    "f" -> List("g"),
    "g" -> List.empty[String]
  )

  println(s"""
       |----------------------------------
       |sortGraphRecursive
       |${sortGraphRecursive(digraph)}
       |----------------------------------
       |sortIterative
       |${sortIterative(digraph)}
       |----------------------------------
       |sortTailRec
       |${sortTailRec(digraph)}
       |----------------------------------
       |sortTailRecMoreFunctional
       |${sortTailRecMoreFunctional(digraph)}
       |----------------------------------
       |""".stripMargin)
}
