package insynth.util.logging

import com.dongxiguo.zeroLog.Filter
import com.dongxiguo.zeroLog.formatters.SimpleFormatter

import scala.util.logging.{ Logged, ConsoleLogger }

/** 
 * Classes can mix this trait for having access to the "default" {{{logger}}}.
 *  
 * Clients can inject different loggers if needed.
 */
trait HasDefferLogger {  
  
  protected[this] def getMyClass = this.getClass
      
  protected[this] lazy val (logger, formatter) =
  	StringLoggerFactory.newLogger(getMyClass.toString)
//    	(Filter.Off, new SimpleFormatter(getMyClass.toString) with ConsoleLogger)
//    (new DummyLogger, null)
  
	import HasDefferLogger._
  import formatter._
  
  def warning(msg: => String) =
  	if (loggingCondition)
  		logger.warning(this.toString + ": " + msg)        		   
    
  def severe(msg: => String) = 
  	if (loggingCondition)
  		logger.severe(this.toString + ": " + msg)
     
  def info(msg: => String) = 
  	if (loggingCondition)
  		logger.info(this.toString + ": " + msg)
   
  def fine(msg: => String) = 
  	if (loggingCondition)
  		logger.fine(this.toString + ": " + msg)
   
  def finer(msg: => String)  = 
  	if (loggingCondition)
  		logger.finer(this.toString + ": " + msg)
   
  def finest(msg: => String) = 
  	if (loggingCondition)
  		logger.finest(this.toString + ": " + msg)
     
  def entering(method: => String, arguments: Any*) =	  
  	if (loggingCondition)
  		logger.finest(toString + ": " + "Entering " + getMyClass + "." + method)
     
  def exiting(method: => String, result: => String) =	  
  	if (loggingCondition)
  		logger.finest(toString + ": " + "Exiting " + getMyClass + "." + method + " with " + result)
  		
  def startLogging = {
  	StringLogger.init
	  loggingCondition = true
	}
    
  def stopLogging(flush: Boolean = false) = {
	  if (flush)
	    Console.println(StringLogger.sw.toString)
	  loggingCondition = false
	}
}

object HasDefferLogger {
  var loggingCondition = false  
}