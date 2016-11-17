package com.ir

import scala.collection.mutable
import scala.io.{Source, StdIn}

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

  val inverted = mutable.HashMap[String, Array[Int]]()

  /**
    *
    * @param args
    */
  def main(args: Array[String]): Unit = {

    var input = ""

    if (args.length == 2) {
      input = args(0)
      val inputWikiTitleFile = args(1)

      readIndex(input)

      while (true) {
        getWikiTitles(inputWikiTitleFile, search(userinput)).foreach(p => println(p._1 + ": " + p._2))
      }
    }
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
    *
    * @param file
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
    *
    * @param doc_list1
    * @param doc_list2
    * @return
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
    *
    * @param doc_lists
    * @return
    */
  def intersect(doc_lists: List[Array[Int]]): Array[Int] = {
    var intersections: Array[Int] = Array[Int]()

    //case for one query token, only
    if (doc_lists.length == 1)
      intersections = doc_lists.head

    //initialize intersection
    for (num <- doc_lists.head) {
      intersections = intersections :+ num
    }

    for (doc_id <- doc_lists.tail) {
      intersections = and(doc_id, intersections)
    }
    intersections
  }

  /**
    *
    * @param query
    * @return
    */
  def search(query: List[String]): Array[Int] = {

    var doc_lists: List[Array[Int]] = List[Array[Int]]()

    try {

      //extract posting lists of query
      for (i <- query.indices) {
        doc_lists ::= inverted(query(i))
      }

      // sort the query term posting lists by the number of corresponding occurrences in the documents
      doc_lists = doc_lists.sortWith(_.length < _.length)
      val intersection = intersect(doc_lists)
      if (intersection.length == 0) throw new Exception
      intersection

    }

    catch {case _: Throwable =>
      print("- No results -\n\nWiki-Search: ")
      search(StdIn.readLine().split("\\s+").toList)
    }
  }

  /**
    *
    * @param file
    * @param queryResults
    * @return
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
    queryResults.foreach(id => titleMatches += id -> titleMap(id) )
    titleMatches.toSeq.sortBy(_._1)
  }

  /**
    *
    * @return
    */
  def help() = {
    println("Usage: ./query-index arg1 [OPTION]")
    println("\t\targ1: INPUT1 - produced text file of inverted indices")
    println("\t\t\t[OPTION] - text file with doc id - title mapping")
    sys.exit()
  }

}
