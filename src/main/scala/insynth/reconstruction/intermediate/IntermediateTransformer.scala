package insynth.reconstruction.intermediate

import scala.annotation.tailrec
import scala.collection.mutable.{
  Map => MutableMap,
  LinkedList => MutableList,
  Set => MutableSet,
  Stack
}

import insynth.reconstruction.intermediate.{ Node => IntermediateNode, _ }
import insynth.structures.{ Node => EnvNode, Function => FunctionType, _ }
import insynth.load.Declaration
import insynth.util.logging.HasLogger
import insynth.util.FreshNameGenerator
import insynth.util.format.{ FormatSuccinctNode, FormatIntermediate, FormatSuccinctType }

/**
 * object which transforms the InSynth tree into an intermediate representation
 * tree
 */
object IntermediateTransformer extends (SimpleNode => IntermediateNode) with HasLogger {

  /** The context is a list of variable names paired with their type. */
  type Context = List[(String, DomainType)]
  val emptyContext = List()

  // recursive edges utilities
  var nodeMap: MutableMap[EnvNode, Set[IntermediateNode]] = _
  var recursiveNodeSet: MutableSet[EnvNode] = _
  var recursiveParamsMap: MutableMap[(EnvNode, DomainType), Set[EnvNode]] = _

  // variable for generating variable names, set to a new object in apply
  var variableGenerator: FreshNameGenerator = _

  def apply(root: SimpleNode) = {
    // initialize data for this traversal
    resetInitialData

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
    val transformResult = transform(root, emptyContext, goalType, Set.empty)

    // after the transform, add recursive edges
    updateRecursiveNodes

    // since sets are returned, we should have only one result for the initial tree
    assert(transformResult.size == 1, "transformResult should have size 1")

    transformResult.head
  }

  /**
   * transform method, called recursively as we descent down the tree
   * @param node current node
   * @param context environment (typing) context
   * @param goalType the type that we want to get
   * @return set of nodes that describe how to generate expression of goalType
   */
  private def transform(node: SimpleNode, oldContext: Context,
    goalType: DomainType, visited: Set[EnvNode]): Set[IntermediateNode] = {
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
     * function type (general domain type) - it applies parameters of fun to the functionNode argument
     * @param fun function type
     * @param functionNode the node which represents the function term
     * @return application node
     */
    def generateApplicationAccordingToFunction(fun: FunctionType,
      functionNode: IntermediateNode): Application = {

      assert(functionNode match { case _: Identifier => true; case _ => false },
        "functionNode should be an identifier, not" + FormatIntermediate(functionNode))

      val goalReturnType = getReturnType(goalType)

      fun match {
        // fun directly returns the needed type just generate the application node
        // without recursive calls
        case FunctionType(params, `goalReturnType`) => {
          fine("generating application for function with parameters: " +
            (params map { _.toString }).mkString(","))

          // get a set of nodes for each parameter type
          val mapOfSetsOfParameterTypes = getMapTypesToParameters(params)
          val paramsSetList: List[Set[IntermediateNode]] =
            params map {
              (_: DomainType) match {
                case st: DomainType => mapOfSetsOfParameterTypes(st)
                case null => Set(NullLeaf).asInstanceOf[Set[IntermediateNode]]
                //case _ => throw new RuntimeException
              }
            }

          fine("paramsSetList: " + (paramsSetList) + " size=" + paramsSetList.size)

          Application(fun, Set(functionNode) :: paramsSetList)
        }
        // fun returns another function to which we need to apply arguments, include
        // a recursive call
        case FunctionType(params, innerFun: FunctionType) =>

          assert(false, "functionNode should be identifier not" + FormatIntermediate(functionNode))
          fine("generating application for function with parameters: " +
            (params map { _.toString }))

          val mapOfSetsOfParameterTypes = getMapTypesToParameters(params)
          val paramsSetList: List[Set[IntermediateNode]] =
            params map {
              (_: DomainType) match {
                case st: DomainType => mapOfSetsOfParameterTypes(st)
                //case null => Set(NullLeaf).asInstanceOf[Set[IntermediateNode]]
              }
            }
          fine("paramsSetList: " + (paramsSetList) + " size=" + paramsSetList.size)

          generateApplicationAccordingToFunction(
            innerFun,
            Application(fun, Set(functionNode) :: paramsSetList))

        // anything else is an error 
        case _ => throw new RuntimeException
      }
    }

    /**
     * given a list of domain types, returns a map from a domain type to
     * a set of nodes which can generate an expression of that type
     * @param parameterList list of types
     * @return map from domain type to a set of nodes
     */
    def getMapTypesToParameters(parameterList: List[DomainType]) = {
      // logging
      fine("paramsList: " +
        (parameterList map { t: DomainType => if (t != null) t.toString else "null" }).mkString(","))
      fine("paramsList filter: " +
        ((parameterList filter { _ != null } distinct) map { t: DomainType => if (t != null) t.toString else "null" }).mkString(","))

      // go through all needed parameters and generate appropriate nodes
      // NOTE we eliminate duplicates in order to avoid redundant computation	
      (Map[DomainType, Set[IntermediateNode]]() /:
        // NOTE filter out null values (can be in place of receivers :( )
        (parameterList filter { _ != null } distinct)) {
          (map, parameterType) =>
            {
              // corresponding InSynth type
              val parameterTypeInSynth = parameterType.toSuccinctType

              // log
              fine("need to find parameter for " + parameterType + " parameterTypeInSynth: "
                + parameterTypeInSynth + " or " + FormatSuccinctType(parameterTypeInSynth))

              // get node with the needed type, deeper in down the tree
              val containerNode = node.getParams(parameterTypeInSynth)

              // helper checking function
              def scanNodesForParametersMap(nodesToCheck: Seq[EnvNode]) = {

                // get all possible terms from each node
                (Set[IntermediateNode]() /: nodesToCheck) {
                  (set, node) =>
                    node match {
                      // if simple node (with the type that we need), recursively transform it                                            
                      case nprime: SimpleNode /*if nprime.getType == parameterTypeInSynth*/ =>
                        for (decl <- nprime.getDecls)
                          assert(declarationHasAppropriateType(decl, parameterType),
                            "IntermediateTransformer:281, declaration should have appropriate type")
                        // add recursively transformed node to set
                        set ++ transform(nprime, context, parameterType, visited + node)

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
              val nonRecursiveSet = scanNodesForParametersMap(nonRecursiveParams.toSeq)

              // check for recursive edges
              if (!recursiveParams.isEmpty) {
                // log
                fine("recursive nodes to check " + recursiveParams)

                // add entry to the recursive list to take care at the end
                recursiveNodeSet += node
                // add this node to the mapping
                if (recursiveParamsMap.contains((node, parameterType)))
                  assert(recursiveParamsMap((node, parameterType)) == recursiveParams.toSet, "recursive params map should be same cached")
                else
                  recursiveParamsMap += ((node, parameterType) -> recursiveParams.toSet)
              }

              // return the update map with inner nodes added to the set of solutions
              map + (parameterType -> nonRecursiveSet)
            }
        }
    }

    /**
     * examines the declarations of the current node and returns the set of terms of
     * the goalType according to those declarations
     * @return set of terms which evaluate to goalType
     */
    def getMatchingTypeFromDeclaration: Set[IntermediateNode] = {

      // check each declaration
      (Set[IntermediateNode]() /: node.getDecls) {
        (set, declaration) =>
          {
            declaration match {
              // the declaration should have the needed type
              case nd: Declaration if declarationHasAppropriateType(nd, goalType) => {
                // check the declaration scala type
                val generatedApplication = nd.getDomainType match {

                  // generate application terms according to this function 
                  case sf: FunctionType =>
                    fine("generating application for " + nd)
                    generateApplicationAccordingToFunction(sf, Identifier(nd.getDomainType, nd))

                  // no need for application, directly return the corresponding identifier
                  case i =>
                    Identifier(i, nd)
                }

                // add generated application to the set
                set + generatedApplication
              }

              // should not happen, such declarations cannot give us type that we need
              case _ => throw new RuntimeException("Failed matching " + declaration +
                " while having goal type " + goalType)

            } // declaration match
          }
      } // (Set[IntermediateNode]() /: node.decls)
    } // getMatchingTypeFromDeclaration:Set[IntermediateNode]

    // check each case of the goal type
    val result: Set[IntermediateNode] = goalType match {

      // goal type is function type, we need to add new abstraction
      case fun @ FunctionType(params, retType) => {
        // compute an appropriate abstraction so that the body can be plugged in
        val (abstractionTermFun, contextDelta) = computeAbstraction(emptyContext, goalType)

        // update context
        context = contextDelta ++ context
        // we return abstraction terms union terms when matching declarations
        Set(abstractionTermFun(getMatchingTypeFromDeclaration))
      }

      // we dont need to add new abstraction
      case _ =>
        // return only the set of terms matching given declarations
        // NOTE no need to scan context here because it is done when parameter search encounters a leaf node
        getMatchingTypeFromDeclaration

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
  def computeAbstraction(outerContext: Context, goalType: DomainType): (Set[IntermediateNode] => Abstraction, Context) = {
    // log
    entering(this.getClass().getName(), "computeAbstraction")
    fine("computing abstraction")

    // "last return type" of the goal type
    val neededReturnType = getReturnType(goalType)

    // match the type that we need to generate and produce an appropriate abstraction
    // term together with new context since the goal type is a function
    goalType match {
      case sf @ FunctionType(params, f: FunctionType) => {
        // create an addition to the current context by inspecting all parameters
        // of the function
        val contextDelta: Context = params map { (variableGenerator getFreshVariableName, _) }
        // recursively compute the inner abstraction and the full context
        val innerAbstractionPair = computeAbstraction(contextDelta ++ outerContext, f)
        // return tuple (function, full context)
        (
          { set: Set[IntermediateNode] =>
            Abstraction(
              sf, (params zip contextDelta) map { pair => Variable(pair._1, pair._2._1) },
              Set(innerAbstractionPair._1(set)))
          },
          innerAbstractionPair._2)
      }
      case sf @ FunctionType(params, `neededReturnType`) => {
        // create an addition to the current context
        val contextDelta: Context = params map { (variableGenerator getFreshVariableName, _) }
        // the recursion ends in this case
        (
          // return abstraction with body calculated recursively  
          Abstraction(
            sf, (params zip contextDelta) map { pair => Variable(pair._1, pair._2._1) },
            _: Set[IntermediateNode]),
            // and a context which will be valid when computing the body
            contextDelta ++ outerContext)
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
  def resetInitialData = {
    nodeMap = MutableMap.empty
    recursiveNodeSet = MutableSet.empty
    recursiveParamsMap = MutableMap.empty

    variableGenerator = new FreshNameGenerator("var_")
  }

  // method that is called after the traversal to update recursive node children
  def updateRecursiveNodes = {
    // assert that only application nodes can have recursive edges
    for (
      node <- recursiveNodeSet;
      correspondingIntermediateNode <- nodeMap(node)
    ) assert(correspondingIntermediateNode.isInstanceOf[Application] && correspondingIntermediateNode.asInstanceOf[Application].tpe.isInstanceOf[FunctionType]
  		|| correspondingIntermediateNode.isInstanceOf[Abstraction] && correspondingIntermediateNode.asInstanceOf[Abstraction].subTrees.exists(_.isInstanceOf[Application]),
      "Only application nodes can have recursive edges, but " + FormatIntermediate(correspondingIntermediateNode) + " has them")

    for (
      node <- recursiveNodeSet;
      val correspondingIntermediateNode <- nodeMap(node);
      val nodesToProcess = (correspondingIntermediateNode match {
      	case appX: Application => List(appX)
      	case Abstraction(_, _, subTrees) => subTrees.filter(_.isInstanceOf[Application])
    	});
      app @ Application(FunctionType(pars, retType), _) <- nodesToProcess
    ) {
      val recursiveParams: List[Set[Node]] =
        Set[Node]() +: (for (parType <- pars) yield {
          //assert(recursiveParamsMap.contains((node, parType)), "intermediate 83")

          if (recursiveParamsMap.contains((node, parType)))
            (Set.empty[IntermediateNode] /: recursiveParamsMap((node, parType))) {
              (resultSet, node) => resultSet ++ nodeMap(node)
            }
          else
            Set.empty[IntermediateNode]
        })

      assert(pars.size + 1 == app.params.size, "intermediate 89")
      assert(recursiveParams.size == app.params.size, "intermediate 90")

      app.recursiveParams = recursiveParams
    }
  }

  /**
   * @param queryType type of context variables to return
   * @return a set of variables in context with a given type
   */
  def getAllTermsFromContext(context: Context, queryType: DomainType): Set[IntermediateNode] =
    (Set[IntermediateNode]() /: context) {
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
  def getAllFunctionsFromContextByInSynthType(context: Context, queryType: SuccinctType): Set[Variable] =
    (Set[Variable]() /: context) {
      (set, contextEntry) =>
        {
          contextEntry match {
            case (name, variableType) if variableType.toSuccinctType == queryType => set + Variable(variableType, name)
            case _ => set
          }
        }
    }
}