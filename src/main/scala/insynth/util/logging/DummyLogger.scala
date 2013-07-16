package insynth.util.logging

class DummyLogger {

  def severe(msg: => String) = Unit

  def warning(msg: => String) = Unit

  def info(msg: => String) = Unit

  def fine(msg: => String) = Unit

  def finer(msg: => String) = Unit

  def finest(msg: => String) = Unit
  
}