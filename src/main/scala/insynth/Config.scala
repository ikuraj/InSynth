package insynth

import java.util.logging._

/**
 * Some global parameters for the InSynth configuration
 */
object Config {
  
  /** timeout for the engine (proof tree production) */
  final val getTimeOutSlot = 500

  // default weight for leave nodes (used in extraction phase)
  val weightForLeaves = 1.5d
    
  // logging
  val inSynthLogger = Logger.getLogger("insynth.library")
  inSynthLogger.setUseParentHandlers(false)
  inSynthLogger.setLevel(Level.ALL)
  
  def setLoggerHandler(handler: Handler) {
    inSynthLogger.addHandler(handler)
  }
    
  def removeLoggerHandler(handler: Handler) {
    inSynthLogger.removeHandler(handler)
  }
  
  // variable declaring number of levels of proof trees to log
  private var _proofTreeLevelToLog = 5
  // getter and setter
  def proofTreeLevelToLog_=(lvl: Int) = _proofTreeLevelToLog = lvl  
  def proofTreeLevelToLog = _proofTreeLevelToLog
  
}