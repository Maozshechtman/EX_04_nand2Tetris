package Ex04
import scala.xml._
import scala.reflect.io.File
class Tokenizing(jackFile:File=null) {
  private final val keyWords= Array("class",
  
  "constructor " ,
  "function " ,
  "method " ,
  "field " ,
  "static " ,
  "var " ,
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
  "+" ,
    "-" ,
    "*" ,
    "/" ,
    "&" ,
    "|" ,
    "<" ,
    ">" ,
    "=" ,
    "~")

  private var tokenArray=Array()
  private def createXMlElement(tokName:String,tok:String):scala.xml.Elem = <tokName>tok</tokName>
  private  def writeXMltokens(elements:Array[Elem]): Unit ={
    val root:Elem= <tokens> elements </tokens>
  }



}
