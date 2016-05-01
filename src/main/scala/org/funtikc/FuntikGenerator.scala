package org.funtikc

import org.funtikc.ast.RootNode


class FuntikGenerator extends ClassLoader {

  def generate(funtikCode: String, filename: String): (Array[Byte], RootNode) = {
    val parser = new FuntikParser
    val rootNode = parser.parse(funtikCode)
    (rootNode.generateByteCode(filename), rootNode)
  }
}
