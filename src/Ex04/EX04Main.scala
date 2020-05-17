package Ex04

import java.io.{File, FileOutputStream, PrintWriter}
import javax.swing._
import scala.io.Source
import  Ex04.Tokenizing
import  Ex04.Parsing
object EX04Main {
def readJackFiles(directory:File): Unit ={
  val files = directory.listFiles().filter(x=>x.getName.endsWith(".jack")) //get all files finsh with "jack"
  for(file<-files){

    val tokensFileName=new File()
  }
}
  def main(args: Array[String]): Unit = {

  }

}
