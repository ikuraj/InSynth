package insynth
package attrgrammar

import scala.collection.mutable.{ Map => MutableMap }

import org.kiama.attribution.Attribution._

import streams._
import reconstruction.stream._

import StreamableAST._

trait Streamables[T] {
  
    // streamables that can be attached with recursive edges
    type LazyStreamable = Streamable[T] with AddStreamable[T]

    val stream : //((Class[_], List[T]) => T) =>
      StreamEl => Streamable[T]
    
    val visited : StreamEl => Set[StreamEl]
    
//    val cachedStreams: StreamEl => Map[StreamEl, Streamable[T]]
    
//    val recursiveParamsMap : StreamEl => Map[StreamEl, (LazyStreamable, Set[T])]
    
}

class StreamablesIml[T](streamBuilder: StreamFactory[T]) extends Streamables[T] {
  
    import streamBuilder._
      
    def getStream(combiner: (Class[_], List[T]) => T, streamEl: StreamEl) = {
      initialize
      this.combiner = combiner
      
      val result = streamEl->(combiner)
      
      postProcess
      
      result
    }
  
    val stream :
      //((Class[_], List[T]) => T) =>
      StreamEl => Streamable[T] =
      dynAttr {
      
//        val result: ((Class[_], List[T]) => T) => StreamEl => Streamable[T] = {
//          combiner => {
            case Single(c, inner) => inner->stream
            case a@Alternation(c, inner) => 
              // get node sets for both recursive and non-recursive edges
              val (recursiveParams, nonRecursiveParams) =
                inner partition { a->visited contains _ }
              
              // transform only non-recursive 
              val nonRecursiveStreamableList =
                nonRecursiveParams map { _->stream }
  
              makeRoundRobbin(nonRecursiveStreamableList)
            case CombinatorN(c, inner) =>          
              
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
              
              makeUnaryStream(paramListStream,
                (list: List[T]) => combiner(c, list)
                , Some(_ + 1))
              
            case Empty => makeEmptyStreamable
  
//          }
//        }
          
//        result
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
        
    var recursiveParamsMap: MutableMap[StreamEl, (LazyStreamable, List[StreamEl])] = _
    var nodeMap: MutableMap[StreamEl, Streamable[T]] = _
    
    var combiner: (Class[_], List[T]) => T = _
      
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