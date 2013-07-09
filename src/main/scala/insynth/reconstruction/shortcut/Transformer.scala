package insynth.reconstruction.shortcut

import scala.annotation.tailrec
import scala.collection.mutable.{
  Map => MutableMap,
  LinkedList => MutableList,
  Set => MutableSet,
  Stack
}

import insynth.reconstruction.{ stream => lambda }
import insynth.reconstruction.stream._
import insynth.{ structures => env }
import insynth.structures.{ SimpleNode, ContainerNode }
import insynth.interfaces.Declaration
import insynth.util.logging.HasLogger
import insynth.util.FreshNameGenerator
import insynth.util.format._
import insynth.util.streams._
import insynth.util.streams.ordered.OrderedSizeStreamable

// dependencies on the domain language
import insynth.leon.{ LeonDeclaration => DomainDeclaration }
import insynth.leon.TypeTransformer

import leon.purescala.TypeTrees.{ TypeTree, FunctionType => Function }
import leon.purescala.{ TypeTrees => domain }

/**
 * object which transforms the InSynth tree into an intermediate representation
 * tree
 */
class Transformer(streamBuilderXXX: StreamFactory[Node])
	extends (env.SimpleNode => Stream[(lambda.Node, Float)]) with HasLogger {
  
  val streamBuilder = new DebugOrderedStreamFactory[Node]
  
  // set the types according to which we are conforming our abstract language
  type DomainType = TypeTree
  // set the types according to which we are conforming our abstract language
  type TreePair = (lambda.Node, Float)
  
  // streamables that can be attached with recursive edges
  type LazyStreamable = Streamable[Node] with AddStreamable[Node]
  // structures for initializing streams that stream recursive nodes
//  var postprocessList: MutableList[(Application, List[LazyStreamable])] = MutableList.empty
  var nodeMap: MutableMap[env.Node, Streamable[Node]] = MutableMap.empty
//  var recursiveNodeSet: MutableSet[env.Node] = _
  var recursiveParamsMap: MutableMap[(env.Node, DomainType), (LazyStreamable, Set[env.Node])] = _

  // will use scala to InSynth type transform
  import TypeTransformer.{ apply => typeTransform }

  /** The context is a list of variable names paired with their type. */
  type Context = List[(String, DomainType)]
  val emptyContext = List()

  // variable for generating variable names, set to a new object in apply
  var variableGenerator: FreshNameGenerator = _

  def apply(root: SimpleNode) = {
    // initialize data for this traversal
    initialize

    // calculate the goal type as being the only parameter in the query node
    val goalType =
      root.getDecls.head match {
        // TODO should be bottom type here
        case DomainDeclaration(_, _, domain.FunctionType(_, retType), _) => retType
        case _ => throw new RuntimeException
      }

    // start the transformation by going from the root node (query node), empty
    // context and trying to find the goal type
    val transformed = transform(root, emptyContext, goalType, Set.empty)
    
    // after the transform, add recursive edges
    postProcess

    println("transformed streamable:\n" + FormatStreamUtils.apply((transformed), 10).toString)
    
    transformed match {
      case os: OrderedSizeStreamable[_] =>
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
    entering(this.getClass().getName(), "transform", node.getDecls.head)
    
    // optimization, if the node has already been transformed
    if (nodeMap contains node) {
      // logging
      finest("node is cached in nodeMap, returning cached version - node: " + FormatSuccinctNode(node))
      return nodeMap(node)
    }

    // save old context
    var context = oldContext

    /**
     * returns an application node which generates the goal type according to the given
     * scala function type - it applies parameters of fun to the functionNode argument
     * @param fun scala function type
     * @param functionNode the node which represents the function term
     * @return application node
     */
    def generateApplicationAccordingToFunction(fun: domain.FunctionType,
      functionNode: Node): Streamable[lambda.Node] = {

      assert(functionNode match { case _: Identifier => true; case _ => false },
        "functionNode should be an identifier, not" + functionNode)

      val goalReturnType = getReturnType(goalType)

      fun match {
        // fun directly returns the needed type just generate the application node
        // without recursive calls
        case domain.FunctionType(params, `goalReturnType`) => {
          fine("generating application for function with parameters: " +
            (params map { t => FormatLeonType(t).toString }).mkString(","))

          // get a set of nodes for each parameter type
          val mapOfSetsOfParameterTypes = getMapTypesToParameters(params)
          val paramsStreams: List[Streamable[Node]] =
            params map {
              (_: DomainType) match {
                case st: DomainType => mapOfSetsOfParameterTypes(st)
                case null => streamBuilder.makeSingletonList(List(NullLeaf)).asInstanceOf[Streamable[Node]]
                //case _ => throw new RuntimeException
              }
            }

          fine("paramsSetList: " + (paramsStreams) + " size=" + paramsStreams.size)

          // make streams of lists of parameter combinations
          val paramListStream: Streamable[List[lambda.Node]] =
            paramsStreams match {
              case Nil => streamBuilder.makeSingletonList(Nil)
              case List(stream) => streamBuilder.makeUnaryStreamList(stream, { el: lambda.Node => List(el) })
              case stream1 :: stream2 :: rest =>
                ((streamBuilder.makeBinaryStream(stream1, stream2) { (el1, el2) => List(el1, el2) }: Streamable[List[lambda.Node]]) /: rest) {
                  (resStream: Streamable[List[lambda.Node]], stream3: Streamable[lambda.Node]) =>
                    streamBuilder.makeBinaryStream(resStream, stream3) { (list, el2) => list :+ el2 }
                }
            }
          
          streamBuilder.makeUnaryStream(paramListStream, {
            params: List[lambda.Node] => Application(fun, functionNode :: params)
          }, functionNode.asInstanceOf[Identifier].decl.getSimpleName, Some(_ + 1))
        }
        // fun returns another function to which we need to apply arguments, include
        // a recursive call
        case domain.FunctionType(params, innerFun: domain.FunctionType) =>

          assert(false, "functionNode should be identifier not" + functionNode)
          fine("generating application for function with parameters: " +
            (params map { t => FormatLeonType(t).toString }))

          val mapOfSetsOfParameterTypes = getMapTypesToParameters(params)
          val paramsStreams: List[Streamable[Node]] =
            params map {
              (_: DomainType) match {
                case st: DomainType => mapOfSetsOfParameterTypes(st)
                //case null => Set(NullLeaf).asInstanceOf[Set[IntermediateNode]]
              }
            }
          fine("paramsSetList: " + (paramsStreams) + " size=" + paramsStreams.size)

          // make streams of lists of parameter combinations
          val paramListStream: Streamable[List[lambda.Node]] =
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
          streamBuilder.makeUnaryStream(paramListStream, {
            params: List[lambda.Node] => Application(fun, functionNode :: params)
          }, functionNode.asInstanceOf[Identifier].decl.getSimpleName,  Some(_ + 1))

        // anything else is an error 
        case _ => throw new RuntimeException
      }
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
        (parameterList map { t: DomainType => if (t != null) FormatLeonType(t).toString else "null" }).mkString(","))
      fine("paramsList filter: " +
        ((parameterList filter { _ != null } distinct) map { t: DomainType => if (t != null) FormatLeonType(t).toString else "null" }).mkString(","))

      // go through all needed parameters and generate appropriate nodes
      // NOTE we eliminate duplicates in order to avoid redundant computation	
      (Map[DomainType, Streamable[Node]]() /:
        // NOTE filter out null values (can be in place of receivers :( )
        (parameterList filter { _ != null } distinct)) {
          (map, parameterType) =>
            {
              // corresponding InSynth type
              val parameterTypeInSynth = typeTransform(parameterType)
              // log
              fine("need to find parameter for " + parameterType + " parameterTypeInSynth: "
                + parameterTypeInSynth + " or " + FormatSuccinctType(parameterTypeInSynth))

              // get node with the needed type, deeper in down the tree
              val containerNode = node.getParams(parameterTypeInSynth)

              // helper checking function
              def scanNodesForParametersMap(nodesToCheck: Seq[env.Node]) = {
                // get all possible terms from each node
                (List[Streamable[Node]]() /: nodesToCheck) {
                  (list, node) =>
                    node match {
                      // if simple node (with the type that we need), recursively transform it                                            
                      case nprime: SimpleNode /*if nprime.getType == parameterTypeInSynth*/ =>
                        for (decl <- nprime.getDecls)
                          assert(declarationHasAppropriateType(decl, parameterType),
                            "IntermediateTransformer:211, declaration should have appropriate type")
                        // add recursively transformed node to set
                        transform(nprime, context, parameterType, visited + node) :: list

                      // should not happen                        
                      // if leaf node search the context
                      //case AbsNode(`parameterTypeInSynth`) => set ++ getAllTermsFromContext(parameterType)
                      case _ => throw new RuntimeException("Cannot go down for type: " + parameterType +
                        " (InSynth: " + parameterTypeInSynth + ")" + " and the node is " + node + "(Container node: " + containerNode + ")")

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
	                // log
	                fine("recursive nodes to check " + recursiveParams.mkString(", "))	
	                
		              // for each parameter set of nodes
	                val paramInitStream =
	                  streamBuilder.makeLazyRoundRobbin(nonRecursiveStreamableList)
	                
	                // add this node to the recursive mapping
	                if (recursiveParamsMap.contains((node, parameterType))) {
	                  assert(recursiveParamsMap((node, parameterType))._2 == recursiveParams.toSet,
                      "recursive params map should be same cached")
	                }
	                else
	                  assert(recursiveParams.toSet.size == recursiveParams.size)
	                  recursiveParamsMap += ((node, parameterType) ->
	                  	(paramInitStream, recursiveParams.toSet))
                  
                  paramInitStream
	              } else	                
	              	streamBuilder.makeRoundRobbin(nonRecursiveStreamableList): Streamable[Node]
	              
              // return the update map with inner nodes added to the set of solutions
              map + (parameterType -> paramsStream)
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
            declaration match {
              // the declaration should have the needed type
              case nd: DomainDeclaration if declarationHasAppropriateType(nd, goalType) => {
                // check the declaration scala type
                val generatedApplication = nd.getDomainType match {
                  // generate application terms according to this function 
                  case sf: domain.FunctionType =>
                    fine("generating application for " + nd)
                    generateApplicationAccordingToFunction(sf, Identifier(nd.getDomainType, nd))

                  // should not happen
                  case domain.Untyped | domain.UnitType =>
                    throw new RuntimeException

                  // no need for application, directly return the corresponding identifier
                  case i =>
                    streamBuilder.makeSingleton(Identifier(i, nd): Node)
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
      case fun @ domain.FunctionType(params, retType) => {
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
          { el: lambda.Node => abstractionTermFun(el) }, "kurac",  Some(_ + 1))        
      }

      // should not happen
      case domain.Untyped =>
        throw new RuntimeException

      // we dont need to add new abstraction
      case _ =>
        // return only the set of terms matching given declarations
        // NOTE no need to scan context here because it is done when parameter search encounters a leaf node        
        assert(getMatchingTypeFromDeclaration.size == node.getDecls.size, "kurac!: " + getMatchingTypeFromDeclaration.size + "==" + node.getDecls.size)
        streamBuilder.makeRoundRobbin(getMatchingTypeFromDeclaration)

    }

    // add transformed node into the result
    assert(!nodeMap.contains(node), "nodeMap should not contain: " + node)
    nodeMap += (node -> result)

    // set of intermediate nodes is stored in result
    result
  }

  // XXX possible grouping of declarations with the same scala type!

  /**
   * computes the corresponding abstraction element for the given goal type
   * @param outerContext context of the outer abstraction
   * @return tuple of function in which body should be plugged in and the generated
   * 	context
   */
  def computeAbstraction(outerContext: Context, goalType: DomainType): (Node => Abstraction, Context) = {
    // log
    entering(this.getClass().getName(), "computeAbstraction")
    fine("computing abstraction")

    // "last return type" of the goal type
    val neededReturnType = getReturnType(goalType)

    // match the type that we need to generate and produce an appropriate abstraction
    // term together with new context since the goal type is a function
    goalType match {
      case sf @ domain.FunctionType(params, f: domain.FunctionType) => {
        // create an addition to the current context by inspecting all parameters
        // of the function
        val contextDelta: Context = params map { (variableGenerator getFreshVariableName, _) }
        // recursively compute the inner abstraction and the full context
        val innerAbstractionPair = computeAbstraction(contextDelta ++ outerContext, f)
        // return tuple (function, full context)
        (
          { node: Node =>
            Abstraction(
              sf, (params zip contextDelta) map { pair => Variable(pair._1, pair._2._1) },
              innerAbstractionPair._1(node))
          }, innerAbstractionPair._2)
      }
      case sf @ domain.FunctionType(params, `neededReturnType`) => {
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
      case domain.Untyped => throw new RuntimeException
      case domain.FunctionType(params, f: domain.FunctionType) => getReturnType(f)
      case domain.FunctionType(params, retType) => getReturnType(retType)
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
    val goalReturnInSynthType = typeTransform(goalType) match {
      case Arrow(_, retType) => retType
      case c: Const => c
      case i: Instance => i
      case _ => throw new RuntimeException
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
            case (name, variableType) if typeTransform(variableType) == queryType => set + Variable(variableType, name)
            case _ => set
          }
        }
    }
}