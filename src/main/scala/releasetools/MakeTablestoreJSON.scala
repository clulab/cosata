package releasetools

import java.io.PrintWriter

import explanationgraph.TableStore

import scala.collection.mutable.ArrayBuffer


object MakeTablestoreJSON {

  def main(args:Array[String]): Unit = {
    val filenameOut = "tablestore.json"

    val tablestoreIndex = "/home/peter/github/Synchronicity/annotation/tablestore/tableindex.txt"
    val tablestore = new TableStore(tablestoreIndex, twoLineHeader = false, inTableSubdir = true)


    val jsonRows = new ArrayBuffer[String]

    for (table <- tablestore.tables) {
      for (row <- table.rows) {
        val rowText = row.toStringText().replaceAll("\"", "'" )
        val uuid = row.uid

        val jsonStr:String = "\t" + "\"" + uuid + "\": \"" + rowText + "\""
        jsonRows.append(jsonStr)
      }
    }

    println (" Exporting " + filenameOut)

    val pw = new PrintWriter(filenameOut)
    pw.println("{")
    pw.println(jsonRows.mkString(",\n"))
    pw.println("}")
    pw.flush()
    pw.close()

  }

}
