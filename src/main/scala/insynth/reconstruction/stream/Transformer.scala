package insynth.reconstruction.stream

import scala.annotation.tailrec

// enable postfix operators
import scala.language.postfixOps

import scala.collection.mutable.{
  Map => MutableMap,
  LinkedList => MutableList,
  Set => MutableSet,
  Stack
}

import insynth.streams._
import insynth.streams.ordered._

import insynth.reconstruction.{ stream => lambda }
import insynth.reconstruction.stream._
import insynth.{ structures => env }
import insynth.structures.{ Function => FunctionType, Node => _, Variable => _, _ }
import insynth.load.Declaration

import org.sietf.logging.HasLogger
import insynth.util.FreshNameGenerator
import insynth.util.format._

/**
 * object which transforms the InSynth tree into an intermediate representation
 * tree
 */
class Transformer(streamBuilder: StreamFactory[Node])
	extends (env.SimpleNode => Stream[(lambda.Node, Float)]) with HasLogger {
  
  // set the types according to which we are conforming our abstract language
  type TreePair = (lambda.Node, Float)
  
  // streamables that can be attached with recursive edges
  type LazyStreamable = Streamable[Node] with AddStreamable[Node]
  // structures for initializing streams that stream recursive nodes
  var nodeMap: MutableMap[env.Node, Streamable[Node]] = MutableMap.empty
  var recursiveParamsMap: MutableMap[(env.Node, DomainType), (LazyStreamable, Set[env.Node])] = _

  /** The context is a list of variable names paired with their type. */
  type Context = List[(String, DomainType)]
  val emptyContext = List()

  // variable for generating variable names, set to a new object in apply
  var variableGenerator: FreshNameGenerator = _

  def apply(root: SimpleNode) = {
    // initialize data for this traversal
    initialize

    // the procedure can be applied to any node (not just query) - calculate the goal type
    // as being the return type of the function
    val goalType =
      root.getDecls.head.getDomainType match {
        case FunctionType(_, retType) => retType
        case _ => throw new Exception("Domain type of the root declaration should reflect a function" +
        		"(since it has to be bot -> desiredType)")
      }

    // start the transformation by going from the root node (query node), empty
    // context and trying to find the goal type
    val transformed = transform(root, emptyContext, goalType, Set.empty)
    
    // after the transform, add recursive edges
    postProcess
    transformedStreamable = transformed
//    val file = new java.io.PrintWriter(new java.io.FileOutputStream("streamable.txt"))
//    file.println("transformed streamable:\n" + FormatStreamUtils2.apply((transformed)/*, 10*/).toString)
//    file.flush()
    
    transformed match {
      case os: OrderedStreamable[_] =>
        fine("returning ordered streamable")
        os.getStream zip os.getValues.map(_.toFloat)
      case _: Streamable[_] =>
        fine("returning unordered streamable")
        transformed.getStream zip Stream.continually(0f)
    }
  }

  /**
   * transform method, called recursively as we descent down the tree
   * @param node current node
   * @param context environment (typing) context
   * @param goalType the type that we want to get
   * @return set of nodes that describe how to generate expression of goalType
   */
  private def transform(node: SimpleNode, oldContext: Context,
    goalType: DomainType, visited: Set[env.Node]): Streamable[lambda.Node] = {
    // logging
    entering("transform", node.getDecls.head)
    
    // optimization, if the node has already been transformed
    if (nodeMap contains node) {
      // logging
      finest("node is cached in nodeMap, returning cached version - node: " + FormatSuccinctNode(node))
      return nodeMap(node)
    }

    // save old context
    var context = oldContext

    // caching of already transformed parameter types
    var _mapTypesToParameters: Map[SuccinctType, Streamable[lambda.Node]] = Map()
    
    // get a stremable for a given type (will be used as a parameter)
    def mapTypesToParameters(parameterType: DomainType) = {
      val parameterTypeInSynth = parameterType.toSuccinctType
      if (_mapTypesToParameters contains parameterTypeInSynth)
        _mapTypesToParameters(parameterTypeInSynth)
      else {
        val parameterStream = mapTypeToParameters(parameterType)
        _mapTypesToParameters += (parameterTypeInSynth -> parameterStream)
        parameterStream
      }
    }

    def mapTypeToParameters(parameterType: DomainType) = {
      val parameterTypeInSynth = parameterType.toSuccinctType
      assert(node.getParams.contains(parameterTypeInSynth))
			val containerNode = node.getParams(parameterTypeInSynth)
      finer("parameterType=" + parameterType + ", parameterTypeInSynth=" + parameterTypeInSynth)
			
      // helper checking function
      def scanNodesForParametersMap(nodesToCheck: Seq[env.Node]) = {
        // get all possible terms from each node
        (List[Streamable[Node]]() /: nodesToCheck) {
          (list, nodeToCheck) =>
            nodeToCheck match {
              // if simple node (with the type that we need), recursively transform it                                            
              case nprime: SimpleNode /*if nprime.getType == parameterTypeInSynth*/ =>
                for (decl <- nprime.getDecls)
                  assert(declarationHasAppropriateType(decl, parameterType),
                    "declaration " + decl + " should have appropriate type " + parameterType)
                // add recursively transformed node to set
                transform(nprime, context, parameterType, 
                    visited + nodeToCheck) :: list

              // should not happen                        
              // if leaf node search the context
              //case AbsNode(`parameterTypeInSynth`) => set ++ getAllTermsFromContext(parameterType)
              case _ => throw new RuntimeException("Cannot go down for type: " + parameterType +
                " (InSynth: " + parameterTypeInSynth + ")" + " and the node is " + nodeToCheck + "(Container node: " + containerNode + ")")

            }
        }
      }

      // get node sets for both recursive and non-recursive edges
      val (recursiveParams, nonRecursiveParams) =
        containerNode.getNodes partition { visited contains _ }

      // transform only non-recursive 
      val nonRecursiveStreamableList = scanNodesForParametersMap(nonRecursiveParams.toSeq)

      val paramsStream =
        // check for recursive edges
        if (!recursiveParams.isEmpty) {
          fine("recursive nodes to check " + recursiveParams.mkString(", "))	
          
          // for each parameter set of nodes
          val paramInitStream =
            streamBuilder.makeLazyRoundRobbin(nonRecursiveStreamableList/*, "lazy params for " + parameterType*/)
          
          // add this node to the recursive mapping
          if (recursiveParamsMap.contains((node, parameterType))) {
            assert(recursiveParamsMap((node, parameterType))._2 == recursiveParams.toSet,
              "recursive params map should be same cached")
            assert(false)
          }
          else {
            assert(recursiveParams.toSet.size == recursiveParams.size)
            recursiveParamsMap += ((node, parameterType) ->
            	(paramInitStream, recursiveParams.toSet))
//	                  recursiveParamsMap += ((node, parameterType) ->
//	                  	(paramInitStream, Set(recursiveParamLast)))
          }
          
          paramInitStream
        } else	                
        	streamBuilder.makeRoundRobbin(nonRecursiveStreamableList/*, "params for " + parameterType + " " + variableGenerator.getFreshVariableName*/): Streamable[Node]
        
      paramsStream
    }
    
    /**
     * returns a streamable of application nodes which generates the goal type according to the given
     * function type - it applies parameters of fun to the functionNode argument
     * @param fun function type
     * @param functionNode the node which represents the function term
     * @return stremable of application nodes
     */
    def generateApplicationAccordingToFunction(fun: FunctionType, functionNode: Node): Streamable[lambda.Node] = {
      entering("generateApplicationAccordingToFunction", fun, functionNode)
      assert(functionNode match { case _: Identifier | _: Variable => true; case _ => false },
        "functionNode should be an identifier, not" + functionNode)

      val goalReturnType = getReturnType(goalType)

      fun match {
        // fun directly returns the needed type just generate the application node
        // without recursive calls
        case FunctionType(params, `goalReturnType`) => {
          // make streams of lists of parameter combinations
          val paramListStream = generateParameters(fun)
          
          // out of this make a stream that streams applications filled with parameters
          streamBuilder.makeUnaryStream(paramListStream, {
            params: List[lambda.Node] => Application(fun, functionNode :: params)
          }/*, functionNode.asInstanceOf[Identifier].decl.getSimpleName*/,  Some(_ + 1))
        }
        
        // fun returns another function to which we need to apply arguments
        case FunctionType(params, innerFun: FunctionType) => {
          val paramListStream = generateParameters(fun)
          
          val innerApplicationStream = streamBuilder.makeUnaryStream(paramListStream, {
            params: List[lambda.Node] => Application(fun, functionNode :: params)
          },  Some(_ + 1))

          generateApplicationAccordingToFunctions(
            innerFun, innerApplicationStream
          )
        }

        // anything else is an error 
        case _ => throw new RuntimeException("matched _ for: " + fun)
      }
    }
    
    /**
     * generates application when multiple function nodes are possible
     */
    def generateApplicationAccordingToFunctions(fun: FunctionType, functionNodes: Streamable[lambda.Node]): Streamable[lambda.Node] = {
      entering("generateApplicationAccordingToFunctions", fun, functionNodes)

      // make streams of lists of parameter combinations
      val paramListStream = generateParameters(fun)
      
      streamBuilder.makeBinaryStream(functionNodes, paramListStream) {
        (functionNode, params) => Application(fun, functionNode :: params)
      }
    }

    /*
     * generates a streamable for list of parameters for application according to function type  
     */
    def generateParameters(fun: FunctionType) = {
      entering("generateParameters", fun)
      val FunctionType(params, goalType) = fun      
      val goalReturnType = getReturnType(goalType)
      
      // get a set of nodes for each parameter type
      val mapOfSetsOfParameterTypes = getMapTypesToParameters(params)
      val paramsStreams: List[Streamable[Node]] =
        params map {
          (_: DomainType) match {
            case st: DomainType => mapOfSetsOfParameterTypes(st)
            //case null => streamBuilder.makeSingletonList(List(NullLeaf)).asInstanceOf[Streamable[Node]]                
          }
        }
      fine("paramsSetList: " + (paramsStreams) + " size=" + paramsStreams.size)

      // make streams of lists of parameter combinations
      val paramListStream: Streamable[List[lambda.Node]] =
        paramsStreams match {
          case Nil => streamBuilder.makeSingletonList(Nil)
          case List(stream) => streamBuilder.makeUnaryStreamList(stream, { el: lambda.Node => List(el) })
          case stream1 :: stream2 :: rest =>
            ((streamBuilder.makeBinaryStreamToList(stream1, stream2/*, "pars for " + functionNode.asInstanceOf[Identifier].decl.getSimpleName*/) { (el1, el2) => List(el1, el2) }: Streamable[List[lambda.Node]]) /: rest) {
              (resStream: Streamable[List[lambda.Node]], stream3: Streamable[lambda.Node]) =>
                streamBuilder.makeBinaryStreamToList(resStream, stream3/*, "pars for " + functionNode.asInstanceOf[Identifier].decl.getSimpleName*/) { (list, el2) => list :+ el2 }
            }
        }
      
      paramListStream
    }

    /**
     * given a list of (Scala) types, returns a map from a Scala type to
     * a set of nodes which can generate an expression of that type
     * @param parameterList list of types
     * @return map from Scala type to a set of nodes
     */
    def getMapTypesToParameters(parameterList: List[DomainType]) = {
      // logging
      fine("paramsList: " +
        (parameterList map { t: DomainType => if (t != null) t.toString else "null" }).mkString(","))
      fine("paramsList filter: " +
        ((parameterList filter { _ != null } distinct) map { t: DomainType => if (t != null) t.toString else "null" }).mkString(","))

      // go through all needed parameters and generate appropriate nodes
      // NOTE we eliminate duplicates in order to avoid redundant computation	
      (Map[DomainType, Streamable[Node]]() /:
        // NOTE filter out null values (can be in place of receivers :( )
        (parameterList filter { _ != null } distinct)) {
          (map, parameterType) =>
            {
              // corresponding InSynth type
              val parameterTypeInSynth = parameterType.toSuccinctType
              // log
              fine("need to find parameter for " + parameterType + " parameterTypeInSynth: "
                + parameterTypeInSynth + " or " + FormatSuccinctType(parameterTypeInSynth))

              // return the update map with inner nodes added to the set of solutions
              map + (parameterType -> mapTypesToParameters(parameterType))
            }
        }
    }

    /**
     * examines the declarations of the current node and returns the set of terms of
     * the goalType according to those declarations
     * @return set of terms which evaluate to goalType
     */
    def getMatchingTypeFromDeclaration: List[Streamable[Node]] = {
      // check each declaration
      (List[Streamable[Node]]() /: node.getDecls) {
        (list, declaration) =>
          {
            fine("getMatchingTypeFromDeclaration: checking decl: " + declaration)
            declaration match {
              // the declaration should have the needed type
              case nd: Declaration if declarationHasAppropriateType(nd, goalType) => {           
                // if abstract then get variables from the context
                lazy val matchingVars = getAllTermsFromContext(context, nd.getDomainType)
                fine("matchingVars=" + matchingVars)
                fine("nd.isAbstract=%b, matchingVars.size > 1 =%b".format(nd.isAbstract, matchingVars.size > 1))
                val generatedApplication = 
	                if (nd.isAbstract && matchingVars.size > 1) {
	                  val variablesStreamable = 
	                    // TODO single stream instead of this - more efficient
	                    streamBuilder.makeRoundRobbin(matchingVars.toSeq map
                        { streamBuilder.makeSingleton(_) })
	                  
		                nd.getDomainType match {
		                  // generate application terms according to this function 
		                  case sf: FunctionType =>
		                    fine("generating application for variable of function type" + sf)
		                    generateApplicationAccordingToFunctions(sf, variablesStreamable)
		                    
		                  // no need for application, directly return the corresponding identifier
		                  case i => variablesStreamable
		                }
	                } else {
	                  val matchingNode =
                  		if (nd.isAbstract) matchingVars.head
                  		else // not abstract, get stream according to declaration type	                  
                  			Identifier(nd.getDomainType, nd)
                  		  
		                nd.getDomainType match {
		                  // generate application terms according to this function 
		                  case sf: FunctionType =>
		                    fine("generating application for " + nd)
		                    generateApplicationAccordingToFunction(sf, matchingNode)
		                    
		                  // no need for application, directly return the corresponding identifier
		                  case i =>
		                    streamBuilder.makeSingleton(matchingNode)
		                }
	                }

                // add generated application to the set
                generatedApplication :: list
              }

              // should not happen, such declarations cannot give us type that we need
              case _ => throw new RuntimeException("Failed matching " + declaration +
                " while having goal type " + goalType)

            } // declaration match
          }
      } // (List[Streamable[Node]]() /: node.getDecls)
    } // getMatchingTypeFromDeclaration

    // check each case of the goal type
    val result: Streamable[Node] = goalType match {

      // goal type is function type, we need to add new abstraction
      case fun @ FunctionType(params, retType) => {
        // compute an appropriate abstraction so that the body can be plugged in
        val (abstractionTermFun, contextDelta) = computeAbstraction(emptyContext, goalType)

        // update context
        context = contextDelta ++ context
                
        // transform all body nodes
        val subtreeStreams = getMatchingTypeFromDeclaration
        // make round robbin out of them
        val roundRobin = streamBuilder.makeRoundRobbin(subtreeStreams.toSeq)
        // out of this make stream of abstractions
        streamBuilder.makeUnaryStream(roundRobin,
          { el: lambda.Node => abstractionTermFun(el) },  Some(_ + 1))        
      }

      // we dont need to add new abstraction
      case _ =>
        // return only the set of terms matching given declarations
        // NOTE no need to scan context here because it is done when parameter search encounters a leaf node        
        if (getMatchingTypeFromDeclaration.size > 0)
        	streamBuilder.makeRoundRobbin(getMatchingTypeFromDeclaration/*, "all decls for goal type" + goalType + " " + variableGenerator.getFreshVariableName*/)
        else
          streamBuilder.makeEmptyStreamable
    }

    // add transformed node into the result
    assert(!nodeMap.contains(node), "nodeMap should not contain: " + node)
    nodeMap += (node -> result)

    exiting("transform", FormatStreamUtils(result).toString)
    // set of intermediate nodes is stored in result
    result
  }

  /**
   * computes the corresponding abstraction element for the given goal type
   * @param outerContext context of the outer abstraction
   * @return tuple of function in which body should be plugged in and the generated
   * 	context
   */
  def computeAbstraction(outerContext: Context, goalType: DomainType): (Node => Abstraction, Context) = {
    entering("computeAbstraction", outerContext, goalType)

    // "last return type" of the goal type
    val neededReturnType = getReturnType(goalType)

    // match the type that we need to generate and produce an appropriate abstraction
    // term together with new context since the goal type is a function
    goalType match {
      case sf @ FunctionType(params, f: FunctionType) => {
        // create an addition to the current context by inspecting all parameters of the function
        val contextDelta: Context = params map { (variableGenerator getFreshVariableName, _) }
        // recursively compute the inner abstraction and the full context
        val innerAbstractionPair = computeAbstraction(contextDelta ++ outerContext, f)

        // return tuple (function, full context)
        (
          { node: Node =>
            	Abstraction(
          			sf, (params zip contextDelta) map { pair => Variable(pair._1, pair._2._1) },
          			innerAbstractionPair._1(node)
        			)
          }, innerAbstractionPair._2
        )
      }
      case sf @ FunctionType(params, `neededReturnType`) => {
        // create an addition to the current context
        val contextDelta: Context = params map { (variableGenerator getFreshVariableName, _) }

        // the recursion ends in this case
        (
          { 
	          // return abstraction with body calculated recursively  
	          Abstraction(
	            sf, (params zip contextDelta) map { pair => Variable(pair._1, pair._2._1) },
	            _)
          },
          // and a context which will be valid when computing the body
          contextDelta ++ outerContext
        )
      }
      // should not happen (if goal type is a function)
      case _ => throw new RuntimeException
    }
  }

  @tailrec /**
   * returns the "last return type" of a scala type
   * @param tpe
   * @return the return (last Const) type of the parameter scala type
   */
  private def getReturnType(tpe: DomainType): DomainType =
    tpe match {
      case FunctionType(params, f: FunctionType) => getReturnType(f)
      case FunctionType(params, retType) => getReturnType(retType)
      case _ => tpe
    }

  /**
   * check if the given declaration and desired type are compatible in terms of the
   * innermost (return) type
   * @param dec declaration to check
   * @param goalType goal type that we are interested in
   * @return true if the declaration can give us the return type we want, otherwise false
   */
  private def declarationHasAppropriateType(dec: Declaration, goalType: DomainType): Boolean = {
    import env._
    
    // get return type of the goalType
    val goalReturnInSynthType = goalType.toSuccinctType match {
      case Arrow(_, retType) => retType
      case c: Const => c
      case i: Instance => i
      case BottomType => BottomType
      case _ => throw new Exception("Cannot handle type: " + goalType.toSuccinctType)
    }
    // get return type of the dec declaration
    val declarationGoalType = dec.getType match {
      case Arrow(_, retType) => Some(retType)
      case _: Const | _: Instance => Some(dec.getType)
      case _ => None
    }

    fine("comparing: " + declarationGoalType + " with " + goalReturnInSynthType)
    // check if declaration is an arrow type and its compatible
    declarationGoalType.isDefined && declarationGoalType.get == goalReturnInSynthType
  }

  // initialize data for each traversal
  def initialize = {
    nodeMap = MutableMap.empty
    recursiveParamsMap = MutableMap.empty

    variableGenerator = new FreshNameGenerator("var_")
  }

  // method that is called after the traversal to update recursive node children
  def postProcess = {
    for (
      ((node, parameterType), (paramInitStream, recursiveParams)) <- recursiveParamsMap
    ) {
      val recursiveStreams = recursiveParams map (nodeMap(_))
      assert(recursiveStreams.size > 0, "recursiveStreams.size > 0")
    
      paramInitStream addStreamable recursiveStreams
      assert(!paramInitStream.isInitialized, "!paramInitStream.isInitialized")
      
      // initialize the lazy round robbin
      paramInitStream.initialize
    }
  }

  /**
   * @param queryType type of context variables to return
   * @return a set of variables in context with a given type
   */
  def getAllTermsFromContext(context: Context, queryType: DomainType): Set[Node] =
    (Set[Node]() /: context) {
      (set, contextEntry) =>
        {
          contextEntry match {
            case (name, `queryType`) => set + Variable(queryType, name)
            case _ => set
          }
        }
    }

  /**
   * @param queryType InSynth type of the variable in the context
   * @return a set of variables in context with a given InSynth type
   */
  def getAllFunctionsFromContextByInSynthType(context: Context, queryType: env.SuccinctType): Set[Variable] =
    (Set[Variable]() /: context) {
      (set, contextEntry) =>
        {
          contextEntry match {
            case (name, variableType) if variableType.toSuccinctType == queryType => set + Variable(variableType, name)
            case _ => set
          }
        }
    }

  // debug, store constructed stream 
  var transformedStreamable: Streamable[lambda.Node] = _
}