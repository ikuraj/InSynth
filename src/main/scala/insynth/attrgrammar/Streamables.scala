package insynth
package attrgrammar

import scala.collection.mutable.{ Map => MutableMap }

import org.kiama.attribution.Attribution
import org.kiama.attribution.Attribution._

import streams._
import reconstruction.stream._
import util.logging._

import StreamableAST._

trait Streamables[T] {
  
    // streamables that can be attached with recursive edges
    type LazyStreamable = Streamable[T] with AddStreamable[T]

    val stream : StreamEl => Streamable[T]
      
    val listStream : ListStreamEl => Streamable[List[T]]
    
    val visited : StreamEl => Set[StreamEl]
    
//    val cachedStreams: StreamEl => Map[StreamEl, Streamable[T]]
    
//    val recursiveParamsMap : StreamEl => Map[StreamEl, (LazyStreamable, Set[T])]
    
}

class StreamablesIml[T](streamBuilder: StreamFactory[T]) extends Streamables[T]
  with HasLogger {

  import streamBuilder._
        
  def getStreamPairs(streamEl: StreamEl,
    process: PartialFunction[(Class[_], T), T],
    combiner: PartialFunction[(Class[_], List[T]), T],
    injections: Map[Class[_], (Stream[T], Boolean)]
  ) = {
    
    getStreamable(streamEl, process, combiner, injections) match {
      case os: OrderedStreamable[_] =>
        fine("returning ordered streamable")
        os.getStream zip os.getValues.map(_.toFloat)
      case us: Streamable[_] =>
        fine("returning unordered streamable")
        us.getStream zip Stream.continually(0f)
    }
    
  }
  
  def getStream(streamEl: StreamEl,
    process: PartialFunction[(Class[_], T), T],
    combiner: PartialFunction[(Class[_], List[T]), T],
    injections: Map[Class[_], (Stream[T], Boolean)]
  ) = {
    getStreamable(streamEl, process, combiner, injections).getStream
  }  
  
  def getStreamable(streamEl: StreamEl,
    process: PartialFunction[(Class[_], T), T],
    combiner: PartialFunction[(Class[_], List[T]), T],
    injections: Map[Class[_], (Stream[T], Boolean)]
  ) = {
    Attribution.initTree(streamEl)
          
    initialize
    this.combiner = combiner
    this.process = process
    this.injections = injections
    
    val transformed = stream(streamEl)
    
    postProcess
    
    transformed
  }
  
  val identityWithClass = (c: Class[_], t: T) => t

  val stream : StreamEl => Streamable[T] =
    dynAttr {
      case Single(c, inner) =>
        val modify: T => T = (t: T) =>
          if (process.isDefinedAt((c, t)))
            process(c, t)
          else
            t
        
        makeUnaryStream( inner->stream, modify ) 
      case Injecter(c) =>
        val (innerStream, isInfinite) = injections(c)
        
        if (isInfinite) makeSingleStream( innerStream )
        else makeFiniteStream( innerStream.toVector )
//        case a@Alternater(c, inner) => 
//          // get node sets for both recursive and non-recursive edges
//          val (recursiveParams, nonRecursiveParams) =
//            inner partition { a->visited contains _ }
//          
//          // transform only non-recursive 
//          val nonRecursiveStreamableList =
//            nonRecursiveParams map { _->stream }
//
//          makeRoundRobbin(nonRecursiveStreamableList)
      case Combiner(c, inner) =>          
                  
        // make streams of lists of parameter combinations
        val paramListStream = inner->listStream
        
        makeUnaryStream(paramListStream,
          (list: List[T]) => combiner(c, list)
          , Some(_ + 1))
        
      case Empty => makeEmptyStreamable
    }
  
//    val cachedStreams: StreamEl => Map[StreamEl, Streamable[T]] =
//      attr {
//        case t if t isRoot => Map( (t, (t->stream)) )
//        case t             => Set(t) | t.parent[StreamEl]->visited
//      }
  
  val visited : StreamEl => Set[StreamEl] =
    attr {
        case t if t isRoot => Set(t)
        case t             => Set(t) | t.parent[StreamEl]->visited
    }
  
  val listStream : ListStreamEl => Streamable[List[T]] =
    attr {
      case agg@Aggregator(clazz, inner) =>
//          val singletonList = makeUnaryStreamList(inner.head->stream, { t: T => List(t) })
        
        val paramsStreams: Seq[Streamable[T]] =
          inner map { _->stream }
        
        // make streams of lists of parameter combinations
        val paramListStream: Streamable[List[T]] =
          paramsStreams match {
            case Nil => makeSingletonList(Nil)
            case List(stream) => makeUnaryStreamList(stream, { el: T => List(el) })
            case stream1 :: stream2 :: rest =>
              ((makeBinaryStream(stream1, stream2) {
                (el1, el2) => List(el1, el2) }: Streamable[List[T]]) /: rest) {
                  (resStream: Streamable[List[T]], stream3: Streamable[T]) =>
                    makeBinaryStream(resStream, stream3) { (list, el2) => list :+ el2 }
                }
          }

        paramListStream
        
      case Generator(clazz, inner) =>
        val nilStream = streamBuilder.makeSingletonList(Nil)
        val genStream = inner->stream
        
        val listStream = streamBuilder.makeLazyRoundRobbinList(List(nilStream))

        val constructorStream =
          makeBinaryStream(listStream, genStream) { (list, el2) => list :+ el2 }
        
        listStream addStreamable constructorStream
        listStream.initialize

        listStream
    }
      
  var recursiveParamsMap: MutableMap[StreamEl, (LazyStreamable, List[StreamEl])] = _
  var nodeMap: MutableMap[StreamEl, Streamable[T]] = _
  
  var combiner: PartialFunction[(Class[_], List[T]), T] = _
  var process: PartialFunction[(Class[_], T), T] = _
  var injections: Map[Class[_], (Stream[T], Boolean)] = _
    
  // initialize data for each traversal
  def initialize = {
    nodeMap = MutableMap.empty
    recursiveParamsMap = MutableMap.empty
  }
  
  // method that is called after the traversal to update recursive node children
  def postProcess = {
    for (
      (streamEl, (paramInitStream, recursiveLinks)) <- recursiveParamsMap
    ) {
      val recursiveStreams = recursiveLinks map ( _->stream )
      assert(recursiveStreams.size > 0, "recursiveStreams.size > 0")
    
      paramInitStream addStreamable recursiveStreams
      assert(!paramInitStream.isInitialized, "!paramInitStream.isInitialized")
      
      // initialize the lazy round robbin
      paramInitStream.initialize
    }
  }
  
//    val recursiveParamsMap : StreamEl => Map[StreamEl, (LazyStreamable, Set[T])] =
//      attr {
//        case Single(c, inner) => inner -> recursiveParamsMap
//        case a@Alternation(c, inner) => 
//        case CombinatorN(c, inner) =>          
//        case Empty => makeEmptyStreamable      
//    }

}