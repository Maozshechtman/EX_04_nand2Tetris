package Ex04

import java.io.{File, FileOutputStream, PrintWriter}

import scala.io.Source

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
    //remove comments
    println("raed form :" + jackFile.getName)
    val code = removeComments(jackFile)
    for (line <- code) println(line)

  }

  private def lineToTokens(line: String) {

  }

  private def removeComments(file: File): Array[String] = {


    def removeSinglecomment(line: String) = if (line.indexOf("//") != -1)
      line.substring(0, line.indexOf("//"))
    else
      line

    def removeBlockComments(strIn: String): String = {
      var startIndex = strIn.indexOf("/**")

      if (startIndex == -1) return strIn

      var result = strIn

      var endIndex = strIn.indexOf("*/")
      if (startIndex == 0 && endIndex == -1) {
        return ""
      }
      while ( {
        startIndex != -1
      }) {

        if (endIndex == -1) return strIn.substring(0, startIndex - 1)
        result = result.substring(0, startIndex) + result.substring(endIndex + 2)
        startIndex = result.indexOf("/**")
        endIndex = result.indexOf("*/")
      }
      return result

    }


    var finalCode = Source.fromFile(file).getLines().map(line => removeSinglecomment(line)).toArray
    finalCode = finalCode.filterNot(x => x.equals("")).toArray
    finalCode = finalCode.map(line => removeBlockComments(line)).toArray


    return finalCode.filterNot(str => str.equals("") || str.startsWith(" *") || str.endsWith("*/"))

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
