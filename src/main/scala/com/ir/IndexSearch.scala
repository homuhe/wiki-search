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


  val input = "src/main/output/small_index.txt"
  val inverted = mutable.HashMap[String, Array[Int]]()

  def main(args: Array[String]): Unit = {

    //input should be the file with the inverted indices produced in 1.1



    println("Please, type in the search terms and press Enter...")
    val query = scala.io.StdIn.readLine()

    //separate by empty space
    val array = query.split("\\s+")
    array.foreach(println)

    extractLines(input)

  }

  def extractLines(file: String) = {
    val lines = Source.fromFile(file)
      .getLines()

    for (line <- lines) {
      val lemma = line.split("\t")(0)
      val indices: Array[Int] = line.split("\t")(1)
                                    .split("\\s+")
                                    .map(element => element.toInt)

      inverted += lemma -> indices
    }

    //only for testing
    for (entry <- inverted) {
      print(s"key: ${entry._1}, indices:")
      entry._2.foreach(number => print(s" $number"))
      println()
    }
  }

}
