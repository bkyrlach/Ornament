package com.kyrlach.ornament.test

import java.io.File
import java.io.FileInputStream
import com.codecommit.antixml.XML
import com.kyrlach.ornament.CSSParser
import com.codecommit.antixml.Elem
import com.codecommit.antixml.Text

object TestTransformation {
  
  def main(args: Array[String]): Unit = {
    val doc = XML.fromInputStream(new FileInputStream(new File("template-test.html")))
    
    val elems = CSSParser.getSelector("#heading1")(doc)
    
    val update = elems.head match {
      case e: Elem => e.addChild(Text("abc"))
    }

    val update2 = elems.updated(0, update)
    println(update2)
    //val doc2 = update2.unselect
    //(em/defsnippet snippet2 "templates/template1.html" ["tbody > *:first-child"] 
//               [fruit quantity] 
//               ["tr > *:first-child"] (em/content fruit)
//               ["tr > *:last-child"] (em/content (str quantity)))
  }
}