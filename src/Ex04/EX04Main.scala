package Ex04

import java.io.File

object EX04Main {
  def readJackFiles(directory: File): Unit = {
    val files = directory.listFiles().filter(x => x.getName.endsWith(".jack")) //get all files finsh with "jack"

    for (file <- files) {
      val x = Tokenizing(file)


    }
  }

  def main(args: Array[String]): Unit = {


  }

  class Tokenizing

}
