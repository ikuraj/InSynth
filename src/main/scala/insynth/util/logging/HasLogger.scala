package insynth.util.logging

/** 
 * Classes can mix this trait for having access to the "default" {{{logger}}}.
 *  
 * Clients can inject different loggers if needed.
 */
trait HasLogger {  
  
  protected[this] def getMyClass = this.getClass
  
  protected[this] lazy val (logger, formatter) = 
    ZeroLoggerFactory.newLogger(getMyClass.toString)
  
  import formatter._
  
  def warning(msg: => String) = logger.warning(msg)        		   
    
  def severe(msg: => String) =	logger.severe(msg)
     
  def info(msg: => String) = logger.info(msg)
   
  def fine(msg: => String) =	logger.fine(msg)
   
  def finer(msg: => String)  = logger.finer(msg)
   
  def finest(msg: => String) = logger.finest(msg)
     
  def entering(method: => String, arguments: Any*) =
	  logger.finest("Entering " + getMyClass + "." + method)
     
  def exiting(method: => String, result: => String) =
	  logger.finest("Exiting " + getMyClass + "." + method + " with " + result)
}