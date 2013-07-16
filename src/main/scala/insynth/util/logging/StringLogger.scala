package insynth.util.logging

import scala.util.logging._
import java.io._

trait StringLogger extends Logged {
  import StringLogger._
  
  override def log(msg: String): Unit = {
    pw.println(msg)
  }
  
  override def toString = sw.toString
  
}

object StringLogger {
  
  def init = {
	  sw = new StringWriter
	  pw = new PrintWriter(sw, true)    
  }
  
  var sw = new StringWriter
  var pw = new PrintWriter(sw, true)
  
}