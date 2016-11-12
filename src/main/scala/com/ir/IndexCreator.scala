package com.ir

import java.io.PrintWriter
import scala.collection.mutable
import scala.io.Source

/**
  * Creates inverted indices of types occurring in resource.
  */

object IndexCreator {

  val fileName1 = "src/main/resources/tubadw-r1-ir-sample-1000"
  val fileName2 = "src/main/output/index.txt"
  val inverted = mutable.HashMap[String, mutable.SortedSet[Int]]()

  def main(args: Array[String]): Unit = {

    createIndices(extractLines(fileName1))
    writeIndices(fileName2)

  }


  def extractLines(file: String) = {
    val lines = Source.fromFile(file)
      .getLines()
      .map(line => line.split("\t"))
    lines
  }

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
