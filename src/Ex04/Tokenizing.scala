package Ex04

import java.io.{File, FileOutputStream, PrintWriter}

import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

case class Tokenizing(jackFile: File = null) {
  //Atribute
  private final val keyWords = Array("class",

    "constructor ",
    "function ",
    "method ",
    "field ",
    "static ",
    "var ",
    "int ",
    "char ",
    "boolean ",
    "void ",
    "true ",
    "false ",
    "null ",
    "this ",
    "let ",
    "do ",
    "if ",
    "else ",
    "while ",
    "return ")
  private final val symbols = Array("{", "}", "(", ")", "[", "]",
    ".",
    ",",
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
    //comment case (move to  the next line)
    for (line <- Source.fromFile(jackFile).getLines()) {
      breakable {
        // case of double back slash
        if (line.startsWith("//")) break
        else if (line.contains("//")) {
          lineToTokens(line.split("//")(0))

        }

        else if (line.startsWith("/*"))
          while (!line.endsWith("*/")) break
        else if (line.contains("/*")) {
          lineToTokens(line.split("/*")(0))
          while (!line.endsWith("*/")) break
        }


      }
    }

  }


  private def lineToTokens(line: String) {

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
