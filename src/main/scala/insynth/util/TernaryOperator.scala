package insynth.util
  
// ternary operator support
case class Bool(b: Boolean) {
  def ?[X](t: => X) = new { 
    def |(f: => X) = if(b) t else f
  }
}

object Bool {
  implicit def BooleanBool(b: Boolean): Bool = Bool(b)
}