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
//	if (className.contains("insynth.reconstruction.stream.DebugOrderedStreamFactory"))
//    	(Filter.Info, new SimpleFormatter(className) with ConsoleLogger)
//    else
//    if (className.contains("insynth.util.streams"))
//    	(Filter.Finest, new SimpleFormatter(className) with ConsoleLogger)
//  	else
//    if (className.contains("Transformer2"))
//    	(Filter.Fine, new SimpleFormatter(className) with ConsoleLogger)
//  	else
////    if (className.contains("Extractor"))
////    	(Filter.Fine, new SimpleFormatter(className) with ConsoleLogger)
////  	else
////    if (className.contains("insynth"))
////    	(Filter.Info, new SimpleFormatter(className) with ConsoleLogger)
////  	else
////    if (className.contains("insynth.InSynth"))
////    	(Filter.Finest, new SimpleFormatter(className) with ConsoleLogger)
////  	else
	if (className.contains("lesynth.Synthesizer"))
    	(Filter.Fine, new SimpleFormatter(className) with ConsoleLogger)
    else
    if (className.contains("lesynth.RelaxedVerifier"))
    	(Filter.Finest, new SimpleFormatter(className) with ConsoleLogger)
    else 
    	(Filter.Off, new SimpleFormatter(className) with ConsoleLogger)

}