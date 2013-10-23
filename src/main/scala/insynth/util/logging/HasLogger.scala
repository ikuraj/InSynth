package insynth.util.logging

/** 
 * Classes can mix this trait for having access to the "default" {{{logger}}}.
 *  
 * Clients can inject different loggers if needed.
 */
trait HasLogger {  

  implicit val implicitThis = this
  
  protected[logging] def getMyClass = this.getClass
  
  protected[this] lazy val logger =
		LoggerFactory.newLogger(this)      		   
    
  def severe(msg: => String) = logger.severe(msg)

  def error(msg: => String) = logger.error(msg)
  
  def warning(msg: => String) = logger.warning(msg)  
     
  def info(msg: => String) = logger.info(msg)
   
  def fine(msg: => String) = logger.fine(msg)
   
  def finer(msg: => String) = logger.finer(msg)
   
  def finest(msg: => String) = logger.finest(msg)
     
  def entering(method: => String, arguments: Any*) =
	  logger.entering(method, arguments: _*)
     
  def exiting(method: => String, result: => String) =
	  logger.exiting(method, result)
}