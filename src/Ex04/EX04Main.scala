package Ex04

import java.io.File

import javax.swing.{JFileChooser, JFrame}

object EX04Main {
  def main(args: Array[String]): Unit = {
    val frame = new JFrame(" Choose directory")
    val fileChooser = new JFileChooser()
    fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
    val res = fileChooser.showOpenDialog(null)
    if (JFileChooser.APPROVE_OPTION == res) {
      val f = fileChooser.getSelectedFile()
      //System.err.println(f.getPath())
    }
    val directory = fileChooser.getSelectedFile.getPath
    readJackFiles(new File(directory))

  }

  def readJackFiles(directory: File): Unit = {
    val files = directory.listFiles().filter(x => x.getName.endsWith(".jack")) //get all files finsh with "jack"

    for (file <- files) {
      val x = Tokenizing(file)
      x.parser()


    }
  }



}
