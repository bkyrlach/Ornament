package com.kyrlach.ornament.test

import javax.xml.parsers.DocumentBuilderFactory
import java.io.File
import com.kyrlach.ornament.Snippet
import com.kyrlach.ornament.HTMLElement
import com.kyrlach.ornament.Transformation

object TestTransformation {
  def main(args: Array[String]): Unit = {
    val factory = DocumentBuilderFactory.newInstance();
    val builder = factory.newDocumentBuilder()
    implicit val doc = builder.parse(new File("template-test.html"))
    
    val snippet2 = Snippet("tbody > *:first-child", { node => (fruit: String, quantity: Int) => 
      node match {
        case elem: HTMLElement => {
          val clone = elem.cloneNode.asInstanceOf[HTMLElement]
          for(first <- clone.firstChildElement;
              last <- clone.lastChildElement) {
            first.content = fruit
            last.content = quantity.toString
          }
          clone
        }
      }
    })
    
    val fruitData = Map("a" -> 1, "b" -> 2, "c" -> 3)
    
    val t1 = new Transformation("tbody", elem => {
      elem.children = fruitData.flatMap(fruit => snippet2(fruit._1, fruit._2)).toList 
      elem
    })
    
    t1(doc)
    
    val document = HTMLElement(doc)
    println(document)
    //(em/defsnippet snippet2 "templates/template1.html" ["tbody > *:first-child"] 
//               [fruit quantity] 
//               ["tr > *:first-child"] (em/content fruit)
//               ["tr > *:last-child"] (em/content (str quantity)))
  }
}