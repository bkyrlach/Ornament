package com.kyrlach.ornament.test

import java.io.File
import java.io.FileInputStream
import com.kyrlach.ornament.CSSParser
import javax.xml.parsers.DocumentBuilderFactory
import com.kyrlach.ornament.XMLEnricher

import com.kyrlach.ornament.Ornament._

object TestTransformation {
  
  def main(args: Array[String]): Unit = {
    val factory = DocumentBuilderFactory.newInstance();
    val builder = factory.newDocumentBuilder()
    val doc = XMLEnricher(builder.parse(new File("template-test.html")))
    
    //println(doc)

    val snippet2 = snippet(doc, "tbody > *:first-child", { nodes => (fruit: String, quantity: Int)  => {
       at(nodes, 
             ("*:first-child", content(fruit)),
             ("*:last-child", content(quantity.toString))) 
     }
    })

    val fruitData = Map("a" -> 1, "b" -> 2, "c" -> 3)

    //content is defined for a Node, String, List<Node>
    val document = at(doc, ("tbody", content(fruitData.flatMap(fruit => snippet2(fruit._1, fruit._2)).toList )))
    println(document)
    
    //(em/defsnippet snippet2 "templates/template1.html" ["tbody > *:first-child"] 
//               [fruit quantity] 
//               ["tr > *:first-child"] (em/content fruit)
//               ["tr > *:last-child"] (em/content (str quantity)))
  }
}