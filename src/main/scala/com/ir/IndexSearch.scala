package com.ir

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



  def main(args: Array[String]): Unit = {

    //input should be the file with the inverted indices produced in 1.1



    println("Please, type in the search terms and press Enter...")
    var query = scala.io.StdIn.readLine()
    println(query)

    //separate by empty space
    var array = query.split("\\s+")
    array.foreach(println)
  }

}
