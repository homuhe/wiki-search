package com.ir

import scala.collection.mutable
import scala.io.{Source, StdIn}

/** Author:       Alexander Hartmann,
  *               Holger Muth-Hellebrandt
  *
  *               TEST
  *
  * Task:         Assignment 1.2
  * Description:  Searches inverted indices file for the given terms.
  */

/**
  * IndexSearch:
  */
object IndexSearch {

  var input = "big_index.txt"
  val inverted = mutable.HashMap[String, Array[Int]]()

  def main(args: Array[String]): Unit = {

    if (args.length == 1) {
      input = args(0)
    }

    //input should be the file with the inverted indices produced in 1.1
    readIndex(input)

    while (true) {
      print("\nPlease, type in the search terms and press Enter: ")
      val query = StdIn.readLine().split("\\s+").toList

      search(query).foreach(doc_id => print(s" $doc_id"))
    }
  }

  def readIndex(file: String) = {
    val lines = Source.fromFile(file).getLines()

    for (line <- lines) {
      val lemma = line.split("\t")(0)
      val indices = line.split("\t")(1)
                                    .split("\\s+")
                                    .map(element => element.toInt)

      inverted += lemma -> indices
    }
  }

  def and(post_list1: Array[Int], post_list2: Array[Int]): Array[Int] = {

    var inter = Array[Int]()
    for (doc1 <- post_list1) {
      for (doc2 <- post_list2) {
        if (doc1 == doc2) {
          inter = inter :+ doc1
        }
      }
    }
    inter
  }

  def intersect(posting_lists: List[Array[Int]]): Array[Int] = {
    var intersections: Array[Int] = Array[Int]()

    //just one query token
    if (posting_lists.length == 1)
      intersections = posting_lists.head

    //initialize intersection
    for (num <- posting_lists.head) {
      intersections = intersections :+ num
    }

    for (doc_id <- posting_lists.tail) {
      intersections = and(doc_id, intersections)
    }
    intersections
  }

  def search(query: List[String]): Array[Int] = {

    //extract posting lists of query
    var query_values: List[Array[Int]] = List[Array[Int]]()
    for (i <- query.indices) {
      query_values ::= inverted(query(i))
    }
    intersect(query_values)
  }
}
