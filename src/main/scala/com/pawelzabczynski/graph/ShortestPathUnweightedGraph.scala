package com.pawelzabczynski.graph

import scala.annotation.tailrec
import scala.collection.mutable.{Queue => MQueue, Set => MSet}

object ShortestPathUnweightedGraph extends App {

  def shortestPathIterative(
      start: String,
      end: String,
      graph: Map[String, List[String]]
  ): List[String] = {
    val que     = MQueue(List(start))
    val visited = MSet.empty[String]

    while (que.nonEmpty) {
      val path = que.dequeue()
      val u    = path.head
      visited.add(u)
      for (v <- graph(u)) {
        if (v == end) {
          return (v :: path).reverse
        } else if (!visited(v)) {
          visited.add(v)
          que.addOne(v :: path)
        }
      }
    }

    Nil
  }

  def shortestPathTailRec(
      start: String,
      end: String,
      graph: Map[String, List[String]]
  ): List[String] = {
    @tailrec
    def loop(
        queue: List[List[String]],
        visited: Set[String]
    ): List[String] = {
      queue match {
        case Nil => Nil
        case path :: tail =>
          path match {
            case u :: _ if u == end => path.reverse
            case u :: _ =>
              graph(u).filterNot(visited) match {
                case Nil => loop(tail, visited)
                case descendants =>
                  loop(
                    tail ++ descendants.map(_ :: path),
                    visited ++ descendants
                  )
              }
          }
      }
    }
    loop(List(List(start)), Set.empty)
  }

  val directedGraph = Map(
    "a" -> List("b", "c", "d"),
    "b" -> List("e"),
    "c" -> List("f"),
    "d" -> List("g"),
    "e" -> List("g"),
    "f" -> List("g"),
    "g" -> List.empty[String]
  )

  val undirectedGraph = Map(
    "a" -> List("b", "c", "d"),
    "b" -> List("a", "e"),
    "c" -> List("a", "f"),
    "d" -> List("a", "g"),
    "e" -> List("b", "g"),
    "f" -> List("g", "g"),
    "g" -> List("e", "f", "d")
  )

  println(s"""
       |----------------------------
       |undirected graph
       |
       |iterative: ${shortestPathIterative("a", "g", undirectedGraph)}
       |tailrec: ${shortestPathTailRec("a", "g", undirectedGraph)}
       |
       |----------------------------
       |directed graph
       |
       |iterative: ${shortestPathIterative("a", "g", directedGraph)}
       |tailrec:   ${shortestPathTailRec("a", "g", directedGraph)}
       |
       |----------------------------
       |""".stripMargin)
}
