package com.kyrlach.ornament.test

import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.XPathFactory
import java.io.File
import javax.xml.xpath.XPathConstants
import org.w3c.dom.NodeList
import com.kyrlach.ornament.CSSParser

object TestParser {
  
  def main(args: Array[String]): Unit = {
    val factory = DocumentBuilderFactory.newInstance();
    val builder = factory.newDocumentBuilder()
    val doc = builder.parse(new File("test.html"))
    
    CSSParser.parseAll(CSSParser.selector, "#test-content4")
    
    val selector = CSSParser.parseAll(CSSParser.selector, "#test-content4").get
    
    println(selector(doc).item(0))
  }
}