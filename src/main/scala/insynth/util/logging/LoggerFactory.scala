package insynth.util.logging

import scala.collection.mutable.{ Map => MutableMap }
import scala.util.logging.{ Logged, ConsoleLogger }

/** 
 * Factory for producing zero-log loggers
 */
object LoggerFactory {
  
  val logDirectory = "log"
  
  val loggerMap: MutableMap[String, Logged] = MutableMap.empty

  /**
   * returns appropriate logger according to the given string
   * e.g. if (className contains "package.clazz")
   * 				(Filter.Info, new SimpleFormatter(className) with StringLogger)
   *     	else
   *      	(Filter.Finest, new SimpleFormatter(className) with ConsoleLogger)
   * @param className name of a class to return the logger for
   * @return logger for the class
   */
  final def newLogger(className: String) =
    (new DummyLogger, null)
//	if (className.contains("insynth.reconstruction.stream.DebugOrderedStreamFactory"))
//    	(Filter.Info, new SimpleFormatter(className) with StringLogger)
//    else
//    if (className.contains("insynth.util.streams"))
//    	(Filter.Finest, new SimpleFormatter(className) with StringLogger)
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