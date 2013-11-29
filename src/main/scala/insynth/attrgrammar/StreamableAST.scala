package insynth
package attrgrammar

/**
 * Language AST for streamables
 */
object StreamableAST {

    import org.kiama.attribution.Attributable
    import org.kiama.util.Positioned

    type Var = String

    abstract class StreamEl extends Attributable with Positioned

    case class Single(c: Class[_], inner: StreamEl) extends StreamEl
//    case class Combinator2(c: Class[_], left: StreamEl, right: StreamEl) extends StreamEl
    case class CombinatorN(c: Class[_], inner: Seq[StreamEl]) extends StreamEl
    case class Alternation(c: Class[_], inner: Seq[StreamEl]) extends StreamEl
    case class Injectable(c: Class[_]) extends StreamEl
    case object Empty extends StreamEl

}
