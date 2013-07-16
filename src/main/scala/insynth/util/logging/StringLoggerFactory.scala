package insynth.util.logging

import com.dongxiguo.zeroLog.Filter
import com.dongxiguo.zeroLog.formatters.SimpleFormatter

import scala.collection.mutable.{ Map => MutableMap }
import scala.util.logging.{ Logged, ConsoleLogger }

/** 
 * Factory for producing zero-log loggers
 */
object StringLoggerFactory {
  
  val logDirectory = "log"
  
  val loggerMap: MutableMap[String, Logged] = MutableMap.empty
  
  final def newLogger(className: String) =
//	if (className.contains("insynth.reconstruction.stream.DebugOrderedStreamFactory"))
//    	(Filter.Info, new SimpleFormatter(className) with StringLogger)
//    else
//    if (className.contains("insynth.util.streams"))
    	(Filter.Finest, new SimpleFormatter(className) with StringLogger)
//  	else
//    if (className.contains("Reconstructor"))
//    	(Filter.Fine, new SimpleFormatter(className) with StringLogger)
//  	else
////    if (className.contains("Extractor"))
////    	(Filter.Fine, new SimpleFormatter(className) with StringLogger)
////  	else
////    if (className.contains("insynth"))
////    	(Filter.Info, new SimpleFormatter(className) with StringLogger)
////  	else
////    if (className.contains("insynth.InSynth"))
////    	(Filter.Finest, new SimpleFormatter(className) with StringLogger)
////  	else
////    if (className.contains("lesynth"))
////    	(Filter.Finest, new SimpleFormatter(className) with StringLogger)
//    else 
//	if (className.contains("lesynth.Synthesizer"))
//    	(Filter.Finest, new SimpleFormatter(className) with StringLogger)
//    else
//    	(Filter.Off, new SimpleFormatter(className) with StringLogger)

}