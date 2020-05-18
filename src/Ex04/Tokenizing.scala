package Ex04

import java.io.{File, FileOutputStream, PrintWriter}

import scala.io.Source
import scala.util.control.Breaks._

class Tokenizing(jackFile: File = null) {
  //Atribute
  private final val keyWords = Array("class",

    "constructor ",
    "function ",
    "method ",
    "field ",
    "static ",
    "var ",
  "int " ,
  "char " ,
  "boolean " ,
  "void " ,
  "true " ,
  "false " ,
  "null " ,
  "this " ,
  "let " ,
  "do " ,
  "if " ,
  "else " ,
  "while " ,
  "return ")
  private final  val symbols=Array("{" , "}" , "(" , ")" , "[" , "]" ,
    "." ,
  "," ,
  ";"
   ,
    "+",
    "-",
    "*",
    "/",
    "&",
    "|",
    "<",
    ">",
    "=",
    "~")
  private val tokenArray = Array[String]()

  def parser(): Unit = {

    for (line <- Source.fromFile(jackFile).getLines()) {
      breakable {
        if (line.startsWith("//")) break

      }

    }


  }

  private def createXMlToken(tokPattren: String, tokName: String): String =
    "<" + tokPattren + ">" + tokName + "</" + tokPattren + ">"

  private def writeXMlTokens(): Unit = {

    val fileName = jackFile.getPath.replaceAll(".jack", "TMaG.xml")
    val root = "<tokens>" + tokenArray.mkString("\n") + "\n" + "</tokens>"
    val writer = new PrintWriter(new FileOutputStream(fileName, true))
    writer.append(root)
    writer.close()

  }


}
