package com.ir

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


  var input = "/home/holger/IdeaProjects/wiki-search/src/main/resources/small_index.txt"
  val inverted = mutable.HashMap[String, Array[Int]]()

  def main(args: Array[String]): Unit = {


    if (args.length == 1) {
      input = args(0)
    }

    //input should be the file with the inverted indices produced in 1.1
    readIndex(input)

    println("Please, type in the search terms and press Enter...")
    val query = scala.io.StdIn.readLine().split("\\s+")

    print(s"Documents found with query (doc id): ")
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

    //only for testing
    /**
    for (entry <- inverted) {
      print(s"key: ${entry._1}, indices:")
      entry._2.foreach(number => print(s" $number"))
      println()
    }**/
  }

  def search(query: Array[String]): Array[Int] = {

    //TODO input is an query split by whitespaces.
    //      Can be by length 1 or more.

    if (query.length == 1) {
      return inverted(query.head)
    }
    else if (query.length >1) {

    }

    def and(doc_ids1: Array[Int], doc_ids2: Array[Int]): Array[Int] = {

      for ()
    }

    for (q <- query) {
      print(s"$q ")
      println(inverted(q).length)
    }

    //INSERT MAGIC HERE

    val bla = Array(2, 3)
    bla
  }

}
