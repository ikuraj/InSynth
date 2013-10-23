package insynth.util.logging

class DummyLogger extends Logger {

  def severe(msg: => String)(implicit hasLogger: HasLogger) = {} 
  
  def error(msg: => String)(implicit hasLogger: HasLogger) = {}

  def warning(msg: => String)(implicit hasLogger: HasLogger) = {}

  def info(msg: => String)(implicit hasLogger: HasLogger) = {}

  def fine(msg: => String)(implicit hasLogger: HasLogger) = {}

  def finer(msg: => String)(implicit hasLogger: HasLogger) = {}

  def finest(msg: => String)(implicit hasLogger: HasLogger) = {}
     
  def entering(method: => String, arguments: Any*)(implicit hasLogger: HasLogger) = {}
     
  def exiting(method: => String, result: => String)(implicit hasLogger: HasLogger) = {}
  
}