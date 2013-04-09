package insynth.util.logging

import com.dongxiguo.zeroLog.Filter
import com.dongxiguo.zeroLog.formatters.SimpleFormatter

import scala.collection.mutable.{ Map => MutableMap }
import scala.util.logging.{ Logged, ConsoleLogger }

/** 
 * Factory for producing zero-log loggers
 */
object ZeroLoggerFactory {
  
  val logDirectory = "log"
  
  val loggerMap: MutableMap[String, Logged] = MutableMap.empty
  
  final def newLogger(className: String) =
    if (className.contains("lesynth.Synthesizer"))
    	(Filter.Off, new SimpleFormatter(className) with ConsoleLogger)
    else
    	(Filter.Off, new SimpleFormatter(className) with ConsoleLogger)

}