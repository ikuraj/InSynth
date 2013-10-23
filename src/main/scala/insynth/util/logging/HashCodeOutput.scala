package insynth.util.logging

trait HashCodeOutput extends Logger {  
  
  override abstract def severe(msg: => String)(implicit hasLogger: HasLogger) = super.severe(hasLogger.hashCode + "# " + msg)

  override abstract def error(msg: => String)(implicit hasLogger: HasLogger) = super.error(hasLogger.hashCode + "# " + msg)
  
  override abstract def warning(msg: => String)(implicit hasLogger: HasLogger) = super.warning(hasLogger.hashCode + "# " + msg)
     
  override abstract def info(msg: => String)(implicit hasLogger: HasLogger) = super.info(hasLogger.hashCode + "# " + msg)

  override abstract def fine(msg: => String)(implicit hasLogger: HasLogger) = super.fine(hasLogger.hashCode + "# " + msg)
   
  override abstract def finer(msg: => String)(implicit hasLogger: HasLogger) = super.finer(hasLogger.hashCode + "# " + msg)
   
  override abstract def finest(msg: => String)(implicit hasLogger: HasLogger) = super.finest(hasLogger.hashCode + "# " + msg)
     
  override abstract def entering(method: => String, arguments: Any*)(implicit hasLogger: HasLogger) = super.entering(hasLogger.hashCode + "# " + method, arguments: _*)
     
  override abstract def exiting(method: => String, result: => String)(implicit hasLogger: HasLogger) = super.exiting(hasLogger.hashCode + "# " + method, result)

}