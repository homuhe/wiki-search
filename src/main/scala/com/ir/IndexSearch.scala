package com.ir

import scala.collection.mutable
import scala.io.Source

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

  var input = "medium_index.txt"
  val inverted = mutable.HashMap[String, List[Int]]()

  def main(args: Array[String]): Unit = {


    if (args.length == 1) {
      input = args(0)
    }

    //input should be the file with the inverted indices produced in 1.1
    readIndex(input)

    print("Please, type in the search terms and press Enter: ")
    val query = scala.io.StdIn.readLine().split("\\s+").toList

    search(query).foreach(doc_id => print(s" $doc_id"))

  }

  def readIndex(file: String) = {
    val lines = Source.fromFile(file).getLines()

    for (line <- lines) {
      val lemma = line.split("\t")(0)
      val indices: List[Int] = line.split("\t")(1)
                                    .split("\\s+")
                                    .map(element => element.toInt)
                                      .toList
      inverted += lemma -> indices
    }
  }

  def search(query: List[String]): List[Int] = {

    var query_values: List[List[Int]] = List[List[Int]]()
    for (i <- query.indices) {query_values ::= inverted(query(i))}

    var results = List[Int]()

    try {
      if (query.length == 1) {
        results = inverted(query.head)
      }
      else {
        results = intersect(query_values)
      }
    }
    catch {case _: Throwable => println("No results for query.")}



    def intersect(doc_ids: List[List[Int]]): List[Int] = {
      var intersections: List[Int] = List[Int]()

      for (num <- doc_ids.head) {
        intersections ::= num
      }
      for (doc_id <- doc_ids.tail) {
        intersections = and(doc_id, intersections)

        def and(l1: List[Int], l2: List[Int]): List[Int] = {
          var inter = List[Int]()
          for (element1 <- l1) {
            for (element2 <- l2) {
              if (element1 == element2) {
                inter ::= element1
              }
            }
          }
          inter
        }
        //intersections = intersections.intersect(doc_id)
      }
      intersections
    }
    results
  }
}
