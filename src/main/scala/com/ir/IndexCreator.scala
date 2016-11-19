package com.ir

import java.io.PrintWriter
import scala.collection.mutable
import scala.io.Source

/** Author:       Alexander Hartmann,
  *               Holger Muth-Hellebrandt
  *
  * Task:         Assignment 1.1
  * Description:  Creates inverted indices of postings lists out of resource.
  */

object IndexCreator {

  val inverted = mutable.HashMap[String, mutable.SortedSet[Int]]()

  /**
    * Main method which takes two arguments:
    * 1)  INPUT: the given wiki file in CONLL-X format
    * 2) OUTPUT: name of output file for the inverted indices created
    */
  def main(args: Array[String]): Unit = {

    var input, output = ""

    if (args.length != 2) help()
    else {
      input = args(0)
      output = args(1)
    }

    println("Processing...")

    createIndices(extractLines(input))
    writeIndices(output)

    println(s"Successly created ${output.split("/").last} - Done!")

  }

  /**
    * Reads input file and separates at tabs
    * @param file: set of German Wikis in tab-separated CONLL-X dependency format
    * @return lines: Iterator over an array of strings
    */
  def extractLines(file: String) = {
    val lines = Source.fromFile(file)
      .getLines()
      .map(line => line.split("\t"))
    lines
  }

  /**
    * Fills a HashMap which maps type of word to document identifier.
    * @param lines: Iterator over an array of strings
    */
  def createIndices(lines: Iterator[Array[String]]) = {
    for (line <- lines) {
      if (line.length > 1) {
        val lemma = line(2)
        val doc_id = line(5).toInt

        if (!inverted.contains(lemma)) {
          inverted += lemma -> mutable.SortedSet(doc_id)
        }
        else
          inverted(lemma) += doc_id.toInt
      }
    }
  }

  /**
    * Writes created HashMap into text file
    * @param file: String for name of output file
    */
  def writeIndices(file: String) = {
    new PrintWriter(file) {
      for ((lem, ids) <- inverted) {
        write(lem + "\t")
        for (id <- ids)
          write(id + " ")
        write("\n")
      }
    }
  }

  /**
    * Help function for correct usage
    */
  def help() = {
    println("Usage: ./create-index arg1 arg2")
    println("\t\targ1: INPUT - wiki text file in CONLL-X format")
    println("\t\targ2: OUTPUT - file in which inverted indices will be written")
    sys.exit()
  }
}