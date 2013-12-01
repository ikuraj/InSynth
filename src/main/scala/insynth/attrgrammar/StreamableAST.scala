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
    case class Combiner(c: Class[_], inner: ListStreamEl) extends StreamEl
    case class Alternater(c: Class[_], inner: Seq[StreamEl]) extends StreamEl
    /** Allows injection of a stream of values at run-time */
    case class Injecter(c: Class[_]) extends StreamEl
    case object Empty extends StreamEl
//    case class Combinator2(c: Class[_], left: StreamEl, right: StreamEl) extends StreamEl
        
    abstract class ListStreamEl extends Attributable with Positioned
    // generates a list of elements from a single stream
    // NOTE this cannot be expressed in the typing system (something like a function with infinite number of
    // parameters of the same type)
    case class Aggregator(c: Class[_], inner: Seq[StreamEl]) extends ListStreamEl
    case class Generator(c: Class[_], inner: StreamEl) extends ListStreamEl
    // will not produce an empty list
//    case class AggregatorNoEmpty(c: Class[_], inner: StreamEl) extends ListStreamEl

}
