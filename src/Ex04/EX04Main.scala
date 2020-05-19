package Ex04
import java.io.File

object EX04Main {
  def main(args: Array[String]): Unit = {
    val name = "C:\\Users\\user\\Desktop\\שנה ג\\סמסטר ב תשפ\\עקרונות שפות תוכנה\\Labs\\nand2tetris\\projects\\10\\Square"
    readJackFiles(new File(name))
  }

  def readJackFiles(directory: File): Unit = {
    val files = directory.listFiles().filter(x => x.getName.endsWith(".jack")) //get all files finsh with "jack"

    for (file <- files) {
      val x = Tokenizing(file)
      x.parser()


    }
  }



}
