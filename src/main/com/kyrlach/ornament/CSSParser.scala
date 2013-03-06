package com.kyrlach.ornament

import scala.util.parsing.combinator.RegexParsers
import java.nio.channels.Selector
import org.w3c.dom.NodeList
import org.w3c.dom.Document
import javax.xml.xpath.XPathFactory
import javax.xml.xpath.XPathConstants

object CSSParser extends RegexParsers {
  val xPathfactory = XPathFactory.newInstance()
  val xpath = xPathfactory.newXPath()
  //            ["#test-content4"] (em/set-style :background "#00dd00" :font-size "10px")
//              ["#test-content5"] (em/set-style :background "#dd0000" :font-size "10px")
//              ["#test-content5"] (em/remove-style :background :font-size)
//              ["#test-content6"] (em/listen :mouseover fade-out)
//              ["#test-content6"] (em/listen :mouseout fade-in)
//              ["#test-remove-listeners"] (em/listen  
//                                   :click 
//                                   #(em/at js/document 
//                                           ["#test-content6"] (em/remove-listeners :mouseover :mouseout)))
//              ["#test-content6_5"] (em/listen :mouseenter fade-out)
//              ["#test-content6_5"] (em/listen :mouseleave  fade-in)
//              ["#test-unlisten"] (em/listen  
//                                   :click 
//                                   #(em/at js/document  
//                                           ["#test-content6_5"] (em/do->
//                                                                  (em/unlisten :mouseenter fade-out)
//                                                                  (em/unlisten :mouseleave fade-in))))
//              ["#click"] (em/listen
//                          :click 
//                          #(em/at js/document
//                               ["#sz-tst"] (em/chain 
//                                             (em/resize 2 30 500)
//                                             (em/resize 200 30 500 test-callback))))
//              ["#delay-click"] (em/listen
//                          :click 
//                          #(em/at js/document
//                               ["#dly-tst"] (em/chain 
//                                             (em/resize 2 30 500)
//                                             (em/delay 2000 (em/resize 200 30 500)))))
//              ["#mclick"] (em/listen  
//                          :click 
//                          #(em/at js/document 
//                               ["#mv-tst"] (em/move 300 305 500 
//                                                    (em/move 0 0 500))))
//              ["#ftest2"] (em/focus)
//              ["#test-from"] (em/listen :click test-from)
//              ["#test-get-text"] (em/listen :click test-get-text)
//              ["#cb1"] (em/set-prop :checked true))
//    
//;(em/defaction test-suite [])
//   
//  
// 
//(defn funtimes [msg]
//  (em/at js/window (em/listen :resize #(ef/log-debug (str "you resized your window:" %))))
//  (em/at js/document
//      [:.heading (attr= :foo "true")] (em/content msg))
//  (em/wait-for-load (test-grid)))   
//                               
//(set! (.-onload js/window) #(funtimes "THIS IS A TEST"))
  
  def byId: Parser[Document => NodeList] = "#" ~> """([^ ]+)""".r ^^ { id => doc => {
    val expr = xpath.compile("//*[@id='" + id + "']")
    expr.evaluate(doc, XPathConstants.NODESET).asInstanceOf[NodeList] 
  }}
  
  def selector: Parser[Document => NodeList] = byId 
}
