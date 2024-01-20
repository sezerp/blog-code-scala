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
      println(queue.size)
      queue match {
        case Nil => Nil
        case path :: tail =>
          path match {
            case u :: _ =>
              graph(u).filterNot(visited) match {
                case Nil                => loop(tail, visited + u)
                case v :: _ if v == end => (v :: path).reverse
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
    "b" -> List("a", "e"),
    "c" -> List("a", "e"),
    "d" -> List("a"),
    "e" -> List("b", "c", "f"),
    "f" -> List("e")
  )

  val undirectedGraph = Map(
    "a" -> List("b", "c", "d"),
    "b" -> List("e"),
    "c" -> List("e"),
    "d" -> List.empty[String],
    "e" -> List("f"),
    "f" -> List.empty[String]
  )

  println(s"""
       |----------------------------
       |undirected graph
       |
       |iterative: ${shortestPathIterative("a", "f", undirectedGraph)}
       |tailrec:   ${shortestPathTailRec("a", "f", undirectedGraph)}
       |
       |----------------------------
       |directed graph
       |
       |iterative: ${shortestPathIterative("a", "f", directedGraph)}
       |tailrec:   ${shortestPathTailRec("a", "f", directedGraph)}
       |
       |----------------------------
       |""".stripMargin)
}
