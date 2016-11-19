package com.ir

import scala.collection.mutable
import scala.io.{Source, StdIn}

/** Author:       Alexander Hartmann,
  *               Holger Muth-Hellebrandt
  *
  * Task:         Assignment 1.2
  * Description:  Processes query of user input on inverted indices file.
  */

object IndexSearch {

  val inverted = mutable.HashMap[String, Array[Int]]()

  /**
    * Main method which takes one obligatory & one optional argument:
    * 1)  INPUT: created inverted indices file from IndexCreator (Assignment 1.1)
    * 2) OPTION: file with document identifier -> wiki title mapping
    */
  def main(args: Array[String]): Unit = {

    var input = ""

    //query with title mapping
    if (args.length == 2) {
      input = args(0)
      val inputWikiTitleFile = args(1)

      readIndex(input)

      while (true) {
        getWikiTitles(inputWikiTitleFile, search(userinput)).foreach(p => println(p._1 + ": " + p._2))
      }
    }
    //query without title mapping
    else if (args.length == 1) {
      input = args(0)

      readIndex(input)

      while (true) {
        println(search(userinput).mkString("\n"))
      }
    }
    else help()

    def userinput = {
      print("\nWiki-Search: ")
      StdIn.readLine().split("\\s+").toList
    }
  }

  /**
    * Reads in created inverted indices file from IndexCreator
    * @param file: inverted indices file with word type -> posting list mapping
    */
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

  /**
    * Enhanced AND function of two arguments (parallel processing)
    * @param doc_list1 1st argument of AND function
    * @param doc_list2 2nd argument of AND function
    * @return returns intersection of 1st & 2nd argument
    */
  def and(doc_list1: Array[Int], doc_list2: Array[Int]): Array[Int] = {

    var inter = Array[Int]()
    var p1i = 0
    var p2i = 0

    while (p1i != doc_list1.length && p2i != doc_list2.length) {
      val doc1 = doc_list1(p1i)
      val doc2 = doc_list2(p2i)

      if (doc1 == doc2) {
        inter = inter :+ doc1
        p1i += 1
        p2i += 1
      }
      else if (doc1 < doc2) {
        p1i += 1
      }
      else {
        p2i += 1
      }
    }
    inter
  }

  /**
    * Calls AND function and stores intersections of query
    * @param doc_lists postings lists of query
    * @return intersections of postings lists
    */
  def intersect(doc_lists: List[Array[Int]]): Array[Int] = {
    var intersections: Array[Int] = Array[Int]()

    //case for one query token, only
    //if (doc_lists.length == 1)
    intersections = doc_lists.head

    println(intersections.length)

    //initialize intersections
//    for (num <- doc_lists.head) {
//      intersections = intersections :+ num
//    }
//    println(intersections.length)


    //call AND function on prev. intersection results as long as arguments are given
    for (doc_id <- doc_lists.tail) {
      intersections = and(doc_id, intersections)
    }
    intersections
  }

  /**
    * Handles query requests
    * @param query user input split at whitespace
    * @return document identifiers from processed query
    */
  def search(query: List[String]): Array[Int] = {

    var doc_lists: List[Array[Int]] = List[Array[Int]]()

    try {

      //extract posting lists of query
      for (i <- query.indices) {
        doc_lists ::= inverted(query(i))
      }

      //sort query term posting lists by length (smallest first)
      doc_lists = doc_lists.sortWith(_.length < _.length)

      val intersection = intersect(doc_lists)
      if (intersection.length == 0) throw new Exception
      intersection

    }
    //new input if no results due to no entry found or no intersection
    catch {case _: Throwable =>
      print("- No results -\n\nWiki-Search: ")
      search(StdIn.readLine().split("\\s+").toList)
    }
  }

  /**
    * Maps document identifiers to Wiki titles
    * @param file mapping file with doc id -> Wiki title stored
    * @param queryResults document identifiers
    * @return Map of doc id -> Wiki title
    */
  def getWikiTitles(file: String, queryResults: Array[Int]) : Seq[(Int, String)] = {
    var titleMap = mutable.HashMap[Int, String]()
    var titleMatches = Map[Int, String]()

    val lines = Source.fromFile(file).getLines()

    for (line <- lines) {
      val doc_id = line.split("\t")(0).toInt
      val title = line.split("\t")(1)

      titleMap += doc_id -> title
    }

    //find matches
    queryResults.foreach(id => titleMatches += id -> titleMap(id))
    titleMatches.toSeq.sortBy(_._1)
  }

  /**
    * Help function for correct usage
    */
  def help() = {
    println("Usage: ./query-index arg1 [OPTION]")
    println("\t\targ1: INPUT1 - produced text file of inverted indices")
    println("\t\t\t[OPTION] - text file with doc id - title mapping")
    sys.exit()
  }

}
