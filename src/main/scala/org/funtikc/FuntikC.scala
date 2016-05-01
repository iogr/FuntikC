package org.funtikC

import java.io.FileOutputStream
import org.funtikc.ast.RootNode

object FuntikC {
  def main(args: Array[String]) {
    if (args.length < 1) {
      println("Usage: FuntikC [-run|-declaim] [FileToSourceCode]")
      return
    }
    val filename = getFilNameFromArgs(args)
    val sourceCode = scala.io.Source.fromFile(filename).mkString
    val a = new FuntikGenerator()
    val classFilename = if (filename.contains('.')) {
      filename.replaceAll("\\.[^.]*$", "")
    }
    else {
      filename
    }
    val (bytecode, root) = a.generate(sourceCode, classFilename)

    val out = new FileOutputStream(classFilename + ".class")
    out.write(bytecode)
    out.close()

    processOption(getCommandFromArgs(args), classFilename, root)

  }
  
  def getFilNameFromArgs(args:Array[String]):String = args.length match {
    case 1 => args(0)
    case 2 => args(1)
    case _ => throw new RuntimeException("Я ХОТЬ И ФОКУСНИК, НО ОБМАНЩИКОВ НЕ ТЕРПЛЮ")
  }

  def getCommandFromArgs(args:Array[String]):String = args.length match {
    case 2 => args(0)
    case 1 => ""
    case _ => throw new RuntimeException("Я ХОТЬ И ФОКУСНИК, НО ОБМАНЩИКОВ НЕ ТЕРПЛЮ")
  }

  def processOption(command:String, argFunc: => String, root: RootNode):Unit = command match {
    case "-run" => Executor.execute(argFunc)
    case "-declaim" => Declaimer.declaim(root, argFunc)
    case _ =>
  }

}
