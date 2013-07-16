//package insynth.util.logging
//
///** 
// * Classes can mix this trait for having access to the "default" {{{}}.
// *  
// * Clients can inject different  if needed.
// */
//trait HasInteractiveLogger extends HasLogger {  
//  
//  def interactivePause = {
//    System.out.println("Press Any Key To Continue...");
//    new java.util.Scanner(System.in).nextLine();
//  }
//  
//  def wrapper(msg: => Unit) {
//    msg
//    interactivePause
//  }
//  
//  override def warning(msg: => String) = wrapper(super.warning(msg))        		   
//    
//  override def severe(msg: => String) = wrapper(super.severe(msg))
//     
//  override def info(msg: => String) = wrapper(super.info(msg))
//   
//  override def fine(msg: => String) = //wrapper(super.fine(msg))
//  {
//    logger.fine(msg)
//    interactivePause
//  }    
//   
//  override def finer(msg: => String) = wrapper(super.finer(msg))
//   
//  override def finest(msg: => String) = wrapper(super.finest(msg))
//     
//  override def entering(method: => String, arguments: Any*) =
//	  wrapper(super.finest("Entering " + getMyClass + "." + method))
//     
//  override def exiting(method: => String, result: => String) =
//	  wrapper(super.finest("Exiting " + getMyClass + "." + method + " with " + result))
//}