package insynth.reconstruction.stream

import scala.collection.mutable.{ Map => MutableMap, LinkedList => MutableList, Set => MutableSet }

import insynth.reconstruction.{ intermediate => int }
import insynth.reconstruction.{ stream => lambda }

import insynth.Config
import insynth.util.streams._
import insynth.util.streams.ordered.OrderedSizeStreamable

import insynth.util.logging.HasLogger
import insynth.util.format.{ FormatIntermediate, FormatStreamUtils }

/**
 * class that extract the needed amount of snippets from the intermediate representation
 */
class Extractor(streamBuilder: StreamFactory[Node])
  extends (int.Node => Stream[(lambda.Node, Float)]) with HasLogger {
  // implicit conversion to lambda nodes
  import Node._

  // default weights
  val weightForLeaves = Config.weightForLeaves

  // streamables that can be attached with recursive edges
  type LazyStreamable = Streamable[Node] with AddStreamable[Node]
  // structures for initializing streams that stream recursive nodes
  var postprocessList: MutableList[(int.Application, List[LazyStreamable])] = MutableList.empty
  var nodeMap: MutableMap[int.Node, Streamable[Node]] = MutableMap.empty

  /**
   * apply method invokes combination of the intermediate representation tree into
   * a single-snippet trees along with their weights (as sum of all used nodes)
   * @param tree parameter tree to be worked on
   * @param numberOfCombinations number of combinations needed
   * @return numberOfCombinations snippets with lowest weight
   */
  def apply(tree: int.Node) = {
    entering("apply", FormatIntermediate(tree))

    info("initializing extractor")
    initialize

    info("transform tree into a streamable")
    // transform tree into a streamable
    val transformed = transform(tree)

    // logging
    info("done transforming")
    finer("before postprocess got transformed:\n" + FormatStreamUtils(transformed))

    postProcess

    // logging
    info("done postProcess")
    finer("got transformed: " + FormatStreamUtils(transformed))
    fine("transformed.isInfinite: " + transformed.isInfinite)

    // debug
    transformedStreamable = transformed

    // logging
    //exiting("apply", ("" /: (result take 1)) { (string, el) => string + "\n" + FormatIntermediate(el) })

    transformed match {
      case os: OrderedSizeStreamable[_] =>
        fine("returning ordered streamable")
        os.getStream zip os.getValues.map(_.toFloat)
      case _: Streamable[_] =>
        fine("returning unordered streamable")
        transformed.getStream zip Stream.continually(0f)
    }
  }

  // returns a stream and a flag that says if it is infinite
  def transform(tree: int.Node): Streamable[lambda.Node] = {
    // logging
    entering("transform", FormatIntermediate(tree))

    // try to speed up
    // TODO make sure equality and hashCode are fine
    //    if (nodeMap contains tree)
    //      return nodeMap(tree)

    val result: Streamable[lambda.Node] =
      tree match {
        // variable (declared previously as an argument), identifier defined in Scala program  
        case _: int.Variable | _: int.Identifier | int.NullLeaf =>
          streamBuilder.makeSingleton(tree: lambda.Node)

        // apply parameters in the tail of params according to the head of params 
        case app @ int.Application(tpe, params) => {
          assert(!(app.params.isEmpty && !app.recursiveParams.isEmpty), "(app.params.isEmpty && !app.recursiveParams.isEmpty) should not happen for any node!")

          // handle a corner case in which parameter sets are empty
          if ((false /: app.params) {
            (res, set) => res || set.isEmpty
          }) {
            val result = streamBuilder.makeEmptyStreamable
            nodeMap += (tree -> result)
            return result
          }

          // only a single application identifier
          assert(app.params.head.size == 1)

          // make according stream for each parameter node
          val paramsStreams =

            // if we have recursive edges
            if (!app.recursiveParams.isEmpty) {
              // logging
              finer("recursiveParams recorded: " + app.recursiveParams.mkString(", "))

              // for each parameter set of nodes
              val paramsInitStreams = for (paramSet <- params) yield {
                // transform each node and make a (lazy, because of recursive edges) round robbin
                val rrList = (for (param <- paramSet) yield transform(param)).toList
                assert(!rrList.isEmpty)

                streamBuilder.makeLazyRoundRobbin(rrList)
              }

              // add this node for postprocessing
              postprocessList :+= (app, paramsInitStreams)
              // return list of round robbins
              paramsInitStreams
            } // if we do not have recursive edges
            else {
              // for each parameter set of nodes
              for (paramSet <- params) yield {
                // transform and partition to infinite and finite streams
                // TODO this partition is not necessary at this point
                val (infiniteStreams, finiteStreams) =
                  (for (param <- paramSet)
                    yield transform(param)) partition { _.isInfinite }
                // make round robbin from that list
                val rrList = finiteStreams.toArray ++ infiniteStreams
                assert(!rrList.isEmpty)

                streamBuilder.makeRoundRobbin(rrList): Streamable[Node]
              }
            }

          // TODO make this array so it is faster?
          // make streams of lists of parameter combinations
          val paramListSream: Streamable[List[lambda.Node]] =
            paramsStreams match {
              case Nil => streamBuilder.makeSingletonList(Nil)
              case List(stream) => streamBuilder.makeUnaryStreamList(stream, { el: lambda.Node => List(el) })
              case stream1 :: stream2 :: rest =>
                ((streamBuilder.makeBinaryStream(stream1, stream2) { (el1, el2) => List(el1, el2) }: Streamable[List[lambda.Node]]) /: rest) {
                  (resStream: Streamable[List[lambda.Node]], stream3: Streamable[lambda.Node]) =>
                    streamBuilder.makeBinaryStream(resStream, stream3) { (list, el2) => list :+ el2 }
                }
            }

          // out of this make a stream that streams applications filled with parameters
          streamBuilder.makeUnaryStream(paramListSream, { params: List[lambda.Node] => Application(tpe, params) })
        }

        // abstraction first creates all of its arguments
        case int.Abstraction(tpe, vars, subtrees) => {
          // transform all body nodes
          val subtreeStreams = subtrees map transform
          // make round robbin out of them
          val roundRobin = streamBuilder.makeRoundRobbin(subtreeStreams.toSeq)
          // out of this make stream of abstractions
          streamBuilder.makeUnaryStream(roundRobin, { el: lambda.Node => Abstraction(tpe, vars.map(x => (x: lambda.Variable)), el) }, Some(_ + 1))
        }
      }

    // add transformed node to the map and return it
    //assert(!nodeMap.contains(tree), "!nodeMap.contains(tree)")
    nodeMap += (tree -> result)
    result
  }

  def initialize = {
    postprocessList = MutableList.empty
    nodeMap = MutableMap.empty
  }

  def postProcess = {
    fine("postProcess called\nGoing into loop with list of size " + postprocessList.size)
    // for each application node and its parameter round robbins
    for ((app, paramstreams) <- postprocessList) {
      // logging
      fine("postProcess of app: " + app.getParams.head.head.asInstanceOf[int.Identifier].decl.getSimpleName)
      finer("(app, paramstreams): " + (app, paramstreams))
      assert(app.recursiveParams.size == app.params.size)
//      assert(app.recursiveParams.size == app.params.size, "For node " + FormatIntermediate(app) +
//        " app.recursiveParams.size is " + app.recursiveParams.size + " while app.params.size is " + app.params.size)
      assert(app.recursiveParams.head.isEmpty, "Extractor 40")

      // this may not hold since we can not have single stream in case of reduced app
      //assert(app.recursiveParams.size == paramstreams.size, "Extractor, app.recursiveParams.size == paramstreams.size")

      // for each parameter get its recursive edges and streams for non-recursive edges
      for ((recParam, paramStream) <- app.recursiveParams zip paramstreams) {
        // if there are some recursive edges for this parameter
        if (!recParam.isEmpty) {
          // get its transformed streams
          val recParamStreams = recParam map { nodeMap(_) }
          assert((recParamStreams & paramStream.getStreams.toSet).isEmpty, "intersection of recParamStreams and paramStream.streams is not empty")

          // add these streams to the lazy round robbin
          paramStream addStreamable recParamStreams
        }

        assert(!paramStream.isInitialized, "Extractor 49")
        // logging
        fine("paramStream initializing")

        // initialize the lazy round robbin
        paramStream.initialize

        // logging
        fine("paramStream initializing")
        fine("paramStream initialized: " + paramStream)

      }
    }
  }

  // debug, store constructed stream 
  var transformedStreamable: Streamable[lambda.Node] = _
}


        
//        val nodesForParams: List[Set[Node]] = 
//          if (app.recursiveParams == Nil) {
//            params
//          } else {
//          	assert(params.size == app.recursiveParams.size, "extractor 82")
//            params zip app.recursiveParams map {
//          	  case (paramSet1, paramSet2) => paramSet1 ++ paramSet2
//        	  }
//          }
//        
//        val paramsStreams = 
//          for (paramSet <- nodesForParams)
//	        	yield {          
//		          val (infiniteStreams, finiteStreams) = 
//		            (for (param <- paramSet)
//		            	yield transform(param)) partition { _.isInfinite }
//		        		          
//		          RoundRobbin(finiteStreams.toArray ++ infiniteStreams)
//	        	}


//		    // check if we have a commutative declaration
//		    val reducedApp = app.params.head.head match {
//		      case int.Identifier(_, dec) => dec.isCommunitative
//		      case _ => false
//		    }
//		    
//		    if (reducedApp && false) {
//		      assert(params.tail.size == 2)
//		      assert(params.tail.distinct.size == 1)
//		      val singleParamSet = params(1)
//		      
//	          val streams = 
//	            (for (param <- singleParamSet)
//	            	yield transform(param))
//		      		      
//	        val paramRoundRobbin = 
//	          if (!app.recursiveParams.isEmpty) {
//		          val irr = streamBuilder.makeLazyRoundRobbin(streams.toList)		          
//		          postprocessList :+= (app, List(irr))
//		          
//		          irr
//		        } else {
//		          streamBuilder.makeRoundRobbin(streams.toArray)		          
//		        }		        
//		      
//	      	val reducedParamsStream = {
//	      	  val bss = streamBuilder.makeBinaryStream(paramRoundRobbin, paramRoundRobbin) { (el1, el2) => List(el1, el2) }
//	      	  //streamBuilder.makeSingleStream(bss.leftStream, false)
//	      	  streamBuilder.makeEmptyStreamable
//	      	}
//	      	
//	      	//streamBuilder.makeUnaryStream(reducedParamsStream, { params: List[AuxNode] => Application(tpe, app.params.head +: (params map { Set(_:Node) })) })
//	      	  streamBuilder.makeEmptyStreamable
//		    } else {