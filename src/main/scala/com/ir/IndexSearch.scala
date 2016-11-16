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
  val inverted = mutable.HashMap[String, Array[Int]]()

  def main(args: Array[String]): Unit = {


    if (args.length == 1) {
      input = args(0)
    }

    //input should be the file with the inverted indices produced in 1.1
    readIndex(input)

    print("Please, type in the search terms and press Enter: ")
    val query = scala.io.StdIn.readLine().split("\\s+")

    search(query).foreach(doc_id => print(s" $doc_id"))

  }

  def readIndex(file: String) = {
    val lines = Source.fromFile(file).getLines()

    for (line <- lines) {
      val lemma = line.split("\t")(0)
      val indices: Array[Int] = line.split("\t")(1)
                                    .split("\\s+")
                                    .map(element => element.toInt)
      inverted += lemma -> indices
    }
  }

  def search(query: Array[String]): List[Int] = {

    var mapvalues: List[Array[Int]] = List[Array[Int]]()
    for (i <- query.indices) {mapvalues ::= inverted(query(i))}

    var results = List[Int]()

    try {
      if (query.length == 1) {
        results = inverted(query(0)).toList
      }
      else {
        results = intersect(mapvalues)
      }
    }
    catch {case _: Throwable => println("No results for query.")}



    def intersect(doc_ids: List[Array[Int]]): List[Int] = {
      var intersections: List[Int] = List[Int]()
      if (intersections.isEmpty) {
        for (num <- doc_ids.head) {
          intersections ::= num
        }
      }
      else {
        for (doc_id <- doc_ids.tail) {
          for (num <- doc_id) {
            intersections.filter(_ == num)
          }
        }
      }
      intersections
    }
    results
  }
}
