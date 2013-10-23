package insynth.util.logging

import scala.collection.mutable.{ Map => MutableMap }

import com.typesafe.scalalogging.log4j.{ Logger => ScalaLogger }
import org.apache.logging.log4j.LogManager

/** 
 * Factory for producing loggers
 */
object LoggerFactory {
  
  val logDirectory = "log"
  
  val loggerMap: MutableMap[String, Logger] = MutableMap.empty

  /**
   * returns appropriate logger according to the given string
   * e.g. if (className contains "package.clazz")
   * 				(Filter.Info, new SimpleFormatter(className) with StringLogger)
   *     	else
   *      	(Filter.Finest, new SimpleFormatter(className) with ConsoleLogger)
   * @param className name of a class to return the logger for
   * @return logger for the class
   */
  final def newLogger(hasLogger: HasLogger) = {
    val className = hasLogger.getMyClass.getName
    
    if (className contains "insynth.streams")
      new ScalaLog4jLogger(ScalaLogger(LogManager.getLogger(className))) with HashCodeOutput
    else
    	new ScalaLog4jLogger(ScalaLogger(LogManager.getLogger(className)))
    //(new DummyLogger, null)
  }

}