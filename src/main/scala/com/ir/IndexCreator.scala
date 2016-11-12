package com.ir

import java.io.PrintWriter
import scala.collection.mutable
import scala.io.Source

/** Author:       Alexander Hartmann,
  *               Holger Muth-Hellebrandt
  * Description:  Creates inverted indices of types occurring in resource.
  */

object IndexCreator {

  var input = "src/main/resources/tubadw-r1-ir-sample-1000"
  var output = "src/main/output/small_index.txt"
  val inverted = mutable.HashMap[String, mutable.SortedSet[Int]]()

  def main(args: Array[String]): Unit = {

    if (args.length == 2) {
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
    * @param file: set of German Wiki in tab-separated CONLL-X dependency format
    * @return lines: Iterator over an array of strings
    */
  def extractLines(file: String) = {
    val lines = Source.fromFile(file)
      .getLines()
      .map(line => line.split("\t"))
    lines
  }

  /**
    * Creates a Hashmap which maps each type to document identifier.
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

}
