package com.ir

import java.util.NoSuchElementException

import scala.collection.mutable
import scala.io.Source

/** Author:       Alexander Hartmann,
  *               Holger Muth-Hellebrandt
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

    println("Please, type in the search terms and press Enter...")
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

    //TODO catch notFound Exception
    //     Can be by length 1 or more.
    var results = List[Int]()

    try {


      if (query.length == 1) {
        results = inverted(query(0)).toList
      }
      else {
        results = intersection(inverted(query(0)), inverted(query(1)))
      }


    }
    catch {case _: Throwable => println("No results for query.")}


    def intersection(doc_ids1: Array[Int], doc_ids2: Array[Int]): List[Int] = {

      var intersections: List[Int] = List[Int]()

      for (doc1_num <- doc_ids1) {
        for (doc2_num <- doc_ids2) {
          if (doc1_num == doc2_num) {
            intersections = doc1_num :: intersections
          }
        }
      }
      intersections
    }
    results
  }
}
