package Ex04

import java.io.{File, FileOutputStream, PrintWriter}

import scala.io.Source
import scala.util.control.Breaks._
case class Tokenizing(jackFile: File = null) {
  //Atribute
  private final val keyWords = Array("class",

    "constructor",
    "function",
    "method",
    "field",
    "static",
    "var",
    "int",
    "char",
    "boolean",
    "void",
    "true",
    "false",
    "null",
    "this",
    "let",
    "do",
    "if",
    "else",
    "while",
    "return")
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
  // regular exprssions
  val symbolReg = "[\\&\\*\\+\\(\\)\\.\\/\\,\\-\\]\\;\\~\\}\\|\\{\\>\\=\\[\\<]"
  val intReg = "[0-9]+"
  val strReg = "\"[^\"\n]*\""
  val idReg = "[\\w_]+"
  private var tokenArray = Array[String]()

  def parser(): Unit = {
    //remove comments
    println("read form :" + jackFile.getName)
    val code = removeComments(jackFile)
    tokenArray = code.map(line => lineToTokens(line)).filterNot(_.equals("**ignore**")).toArray
    println(tokenArray.toSeq)
    writeXMlTokens()

  }

  private def lineToTokens(line: String): String = {
    var isString = false
    if (line.trim().length == 0 || line.equals("\n"))
      return "**ignore**"
    var myTokens = ""
    var buffer = ""
    for (ch <- line) {
      breakable {
        if (ch == '"') {
          buffer += ch
          isString = !isString
          break //move to the next char
        }
        if (isString) {
          buffer += ch
          break // move to the next char
        }

        else if (symbols.contains(ch.toString)) {
          //we reach to symbol in the code
          if (!buffer.isEmpty && buffer.trim.length > 0)
            myTokens += parseBuffertoken(buffer) + "\n"
          myTokens += createSymbolToken(ch) + "\n"
          buffer = ""
        }
        else if (ch == 32)
        //we reach to white space in the code
          {
            if (!buffer.isEmpty && buffer.trim.length > 0)
              myTokens += parseBuffertoken(buffer) + "\n"
            buffer = ""
          }
          else
          buffer = buffer + ch
      }
    }
    return myTokens
  }

  def parseBuffertoken(buffer: String): String = {
    var noSpacesBuffer = buffer.trim
    if (buffer.matches("[0-9]+"))
      return createConstIntegerToken(noSpacesBuffer)
    else if (noSpacesBuffer(0) == '"')
      return createStringConstantToken(noSpacesBuffer.slice(1, buffer.length - 1))
    else
      return createWordToken(noSpacesBuffer)

  }

  def createWordToken(word: String): String = {
    if (keyWords.contains(word))
      return createXMlToken("keyword", word)
    else return createXMlToken("identifier", word)
  }

  def createConstIntegerToken(number: String) = createXMlToken("integerConstant", number)

  def createStringConstantToken(str: String) = createXMlToken("stringConstant", str)

  def createSymbolToken(symbol: Char): String = {
    symbol match {
      case '<' => return createXMlToken("symbol", "&lt;")
      case '>' => return createXMlToken("symbol", "&gt;")
      case '&' => return createXMlToken("symbol", "&amp;")
      case '"' => return createXMlToken("symbol", "&quet;")
      case _ => return createXMlToken("symbol", symbol.toString)
    }
  }

  private def createXMlToken(tokPattren: String, tokName: String): String =
    "<" + tokPattren + "> " + tokName + " </" + tokPattren + ">"

  private def removeComments(file: File): Array[String] = {


    def removeSinglecomment(line: String) = if (line.indexOf("//") != -1)
      line.substring(0, line.indexOf("//"))
    else
      line

    def removeBlockComments(strIn: String): String = {
      var startIndex = strIn.indexOf("/**")
      strIn.length
      if (startIndex == -1) return strIn

      var result = strIn

      var endIndex = strIn.indexOf("*/")
      if (startIndex == 0 && endIndex == -1) {
        return ""
      }
      while (
        startIndex != -1
      ) {

        if (endIndex == -1) return strIn.substring(0, startIndex - 1)
        result = result.substring(0, startIndex) + result.substring(endIndex + 2)
        startIndex = result.indexOf("/**")
        endIndex = result.indexOf("*/")
      }
      return result

    }


    var finalCode = Source.fromFile(file).getLines().map(line => removeSinglecomment(line)).toArray
    finalCode = finalCode.filterNot(x => x.equals("")).toArray
    finalCode = removeBlockComments(finalCode.mkString("\n")).split("\n")

    return finalCode.filter(_.nonEmpty)

  }

  private def writeXMlTokens(): Unit = {

    val fileName = jackFile.getPath.replaceAll(".jack", "TMaG.xml")
    val root = "<tokens>" + "\n" + tokenArray.mkString("") + "</tokens>\n"

    val writer = new PrintWriter(new FileOutputStream(fileName, true))
    writer.append(root)
    writer.close()

  }


}
