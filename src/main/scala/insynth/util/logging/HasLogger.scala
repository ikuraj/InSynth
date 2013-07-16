package insynth.util.logging

import com.dongxiguo.zeroLog.Filter
import com.dongxiguo.zeroLog.formatters.SimpleFormatter

import scala.util.logging.{ Logged, ConsoleLogger }

/** 
 * Classes can mix this trait for having access to the "default" {{{logger}}}.
 *  
 * Clients can inject different loggers if needed.
 */
trait HasLogger {  
  
  protected[this] def getMyClass = this.getClass
    
  lazy val loggingCondition = true
  
  protected[this] lazy val (logger, formatter) =
    if (loggingCondition)
    	ZeroLoggerFactory.newLogger(getMyClass.toString)
  	else
    	(Filter.Off, new SimpleFormatter(getMyClass.toString) with ConsoleLogger)
//    (new DummyLogger, null)
  
  import formatter._
  
  def warning(msg: => String) = logger.warning(this.toString + ": " + msg)        		   
    
  def severe(msg: => String) = logger.severe(this.toString + ": " + msg)
     
  def info(msg: => String) = logger.info(this.toString + ": " + msg)
   
  def fine(msg: => String) = logger.fine(this.toString + ": " + msg)
   
  def finer(msg: => String)  = logger.finer(this.toString + ": " + msg)
   
  def finest(msg: => String) = logger.finest(this.toString + ": " + msg)
     
  def entering(method: => String, arguments: Any*) =
	  logger.finest(toString + ": " + "Entering " + getMyClass + "." + method)
     
  def exiting(method: => String, result: => String) =
	  logger.finest(toString + ": " + "Exiting " + getMyClass + "." + method + " with " + result)
}