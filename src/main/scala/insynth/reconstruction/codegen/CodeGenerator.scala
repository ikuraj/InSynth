package ch.epfl.insynth.reconstruction.codegen

import ch.epfl.insynth.reconstruction.intermediate._
import ch.epfl.insynth.trees
import ch.epfl.scala.{ trees => Scala }
import ch.epfl.insynth.print._
import ch.epfl.insynth.reconstruction.combinator.{ NormalDeclaration, AbsDeclaration }

import scala.text.Document
import scala.text.Document.empty

/**
 * class that converts an intermediate tree into a list of output elements (elements
 * capable of Scala code generation)
 */
object CodeGenerator extends (Node => List[CodeGenOutput]) {
  
  import FormatHelpers._
  import Document._

  /**
   * takes the tree and calls the recursive function and maps documents to Output elements
   * @param tree root of intermediate (sub)tree to transform
   * @return list of output (code snippet) elements
   */
  def apply(tree:Node) = {
    tree match {
      case Application(Scala.Function(_, _ /* BottomType */), queryDec :: List(set)) =>
      	transform(set.toList, TransformContext.Expr) map { CodeGenOutput(_:Document) }
      case _ => throw new RuntimeException
    }    
  }
  
  object TransformContext extends Enumeration {
    type TransformContext = Value
    val Expr, App, Par, Arg, SinglePar = Value
  }
  import TransformContext._
  
  /**
   * main method (recursive) for transforming a intermediate (sub)tree
   * @param tree root node of the (sub)tree to transform 
   * @return list of documents containing all combinations of available expression for
   * the given (sub)tree
   */
  def transform(tree: Node, ctx: TransformContext = Expr): List[Document] = {
    
    // a local variable to set if parentheses are needed
    var parenthesesRequired = true    
    // do parentheses for parameters if needed
    def doParenApp(appId: Document, params: Document) = {
      if (parenthesesRequired) 
        (appId :: paren(params)) 
	  else
	    (appId :/: params) 
    }        
    def doParenRecApp(receiverDoc: Document, appId: Document, params: Document) = {
      // to match the empty document
      val emptyMatch = empty
      receiverDoc match {
        //case scala.text.DocNil =>
        case `emptyMatch` =>
//	      if (parenthesesRequired) 
          	// if receiver is empty then we always need this form
	        appId :: paren(params)
//		  else
//		    appId :/: params        
        case _:Document =>
	      if (parenthesesRequired) 
	        (receiverDoc :: "." :: appId :: paren(params)) 
		  else
		    (receiverDoc :/: appId :/: params)
      }
    }        
    def doParen(d: Document) = if (parenthesesRequired) paren(d) else d
    
    tree match {
      // variable (declared previously as arguments)
      // NOTE case when it is an argument (type is not needed)
      //case Variable(tpe, name) if ctx==Arg => List ( group(name :: ": " :: transform(tpe) ) )
      case Variable(tpe, name) => List ( name )
      // identifier from the scope
      case Identifier(tpe, dec) => 
        List(dec.getSimpleName)
      // apply parameters in the tail of params according to the head of params 
      case Application(tpe, params) => {
        // so far as we constructed, there should be only one application definition term
        // (which tells us, whether it is a function a method...)
        assert(params.head.size == 1)
                
        // import ternary operator
        import Bool._
        
        // most usual transformation in which the head is a function
        def firstTermFunctionTransform = {
          // set the recursive transformation context
          val recCtx = if (params.tail.size == 1) SinglePar else Par
          // go through all possible transformations of functions
          (List[Document]() /: transform(params.head.head, App)) {
              (list, appIdentifier) =>
        	  	// get every possible parameter combination
                (list /: getParamsCombinations(params.tail, recCtx)) {
            	  (list2, paramsDoc) => list2 :+
    			    group( doParenApp(appIdentifier, paramsDoc) )		      
                }
          }
        }
        
        // match the application definition term
        params.head.head match {
          case Identifier(_, NormalDeclaration(decl))  => {
	        // transform the application identifier in an application context
	        val appIdentifier = transform(params.head.head, App).head
	        
	        /* check what the declaration says about the application */
	        
	        // if inheritance function, just recursively transform
	        if (decl.isInheritanceFun) {
	          assert(params.size == 2)
	          return transform(params(1).toList, ctx)
	        }	        
	        
	        // if literal just return simple name
	        if (decl.isLiteral) {
	          assert(params.size == 1)
	          return List(decl.getSimpleName)
	        }
	        
	        // constructor call
	        // NOTE cannot be curried?
	        if (decl.isConstructor) {
	          assert(params(1).size == 1)
	          assert(params(1).head == NullLeaf)
	          // set if we need parentheses
	          parenthesesRequired = 
	            // if there are any parameters or ctx is as receiver (App)
	            params.drop(2).size > 0 || ctx == App
        	  // go through all combinations of parameters documents
    		  return (List[Document]() /: getParamsCombinations(params.drop(2))) {
	    		(list, paramsDoc) => list :+
				  group("new" :/: doParenApp(appIdentifier, paramsDoc))
			  }	
	        }
	        // method is on some object
	        if (decl.belongsToObject) {	
	          assert(params(1).size == 1)
	          assert(params(1).head == NullLeaf)     
        	  // get info about parameters
        	  val paramsInfo = decl.scalaType match {
	        	case Scala.Method(_, params, _) => params
	        	case _ => throw new RuntimeException("Declared method but scala type is not")
        	  }
	          // set if we need parentheses
	          parenthesesRequired = 
	            // if we have more than one parameter or this term is a single parameter
	            // to outer application
	            params.drop(2).size != 1 || ctx == SinglePar
        	  // go through all combinations of parameters documents
    		  return (List[Document]() /: getParamsCombinations(params.drop(2), paramsInfo, parenthesesRequired)) {
	    		(list, paramsDoc) => list :+
				  // TODO when to generate dot and when not??
				  //group(decl.getObjectName :: "." :: doParen(appIdentifier, paramsDoc))
				  group(doParenRecApp(decl.getObjectName, appIdentifier, paramsDoc))
			  }	
	        }	          
	        
            // TODO refactor - similar to the method construction 
	        if (decl.isField) {
	          assert(params.size == 2)
	          // if the field needs this keyword
	          val needsThis = decl.hasThis
	          (List[Document]() /: params(1)) {
        	    (listDocsReceivers, receiver) => {
    	    	  // get documents for the receiver objects (or empty if none is needed)
    	    	  val documentsForThis = {
    			    if (!needsThis)
			    	  receiver match {
			          	case Identifier(_, NormalDeclaration(receiverDecl)) if receiverDecl.isThis =>
		          		  List(empty)
			          	case _ => transform(receiver, App) map { (_:Document) :: "." }			            
		        	  }
    			    else transform(receiver, App) map { (_:Document) :: "." }
    	    	  }	
    	    	  // go through all the receiver objects and add to the list
    	    	  (listDocsReceivers /: documentsForThis) {
	    		    (listDocsTransformedReceiver, receiverDoc) => {
	    		      listDocsTransformedReceiver :+ group(receiverDoc :: appIdentifier)	    		      
	    		    }
    	    	  }
        	    }	        	  
	          }	          	          
	        } 
	          	        
	        else if (!decl.isMethod) {
	          assert(!decl.isConstructor)
        	  // just a function
	          parenthesesRequired = params.tail.size >= 1 || ctx == SinglePar
        	  firstTermFunctionTransform
	        }
	        else // if (decl.isMethod)
	          {
	        	// TODO solve parentheses here (with currying)
	        	// get info about parameters
	        	val paramsInfo = decl.scalaType match {
	        	  case Scala.Method(_, params, _) => params
	        	  case _ => throw new RuntimeException("Declared method but scala type is not")
	        	}
	        	// currying will handle parentheses if needed
	        	parenthesesRequired = params.drop(2).size != 1 || ctx == SinglePar || ctx == App
	        	// if the method needs this keyword
	        	val needsThis = decl.hasThis
	        	(List[Document]() /: params(1)) {
			      (listDocsReceivers, receiver) => {
			        // get documents for the receiver objects (or empty if none is needed)
			        val documentsForThis = {
			          if (!needsThis)
		        		receiver match {
				          case Identifier(_, NormalDeclaration(receiverDecl)) if receiverDecl.isThis =>
				            List(empty)
				          case _ => transform(receiver, App)// map { (_:Document) :: "." }			            
				        }
			          else transform(receiver, App)// map { (_:Document) :: "." }
			        }
				    // go through all the receiver objects and add to the list
				    (listDocsReceivers /: documentsForThis) {
				      (listDocsTransformedReceiver, receiverDoc) => {
				        // go through all combinations of parameters documents
			    		(listDocsTransformedReceiver /: getParamsCombinations(params.drop(2), paramsInfo, parenthesesRequired)) {
			    		  // and add them to the list
			    		  (listDocsTransformedParameters, paramsDoc) => listDocsTransformedParameters :+
						    group(doParenRecApp(receiverDoc, appIdentifier, paramsDoc))
			    		}		      
				      }
				    }
			      }	        	  
	        	}
          	  }
			}
          // function that is created as an argument or anything else
          case Identifier(_, _:AbsDeclaration) | _:Variable | _ =>
          	firstTermFunctionTransform
        }
      }
      // abstraction first creates all of its arguments
      case Abstraction(tpe, vars, subtrees) =>
        assert(vars.size > 0)
        // check if we need parentheses for variables
        parenthesesRequired = vars.size > 1
        // for all bodies of this abstraction
        val abstractionResults = (List[Document]() /: subtrees) {
    	  (listOfAbstractions, body) => {
    	    listOfAbstractions ++
    	    // for all transformations of bodies
    	    (List[Document]() /: transform(body)) {
    	      (listOfBodies, transformedBody) =>
    	    	listOfBodies :+ (
    	    	  // transform argument variables
    			  doParen(seqToDoc(vars, ",", { v:Variable => transform(v, Arg).head })) 
    			  :/: "=>" :/:
    			  // transform the body
				  transformedBody
    			)
    	    }
    	  }
        }
        // return abstraction results
        // we need brackets only if this abstraction is parameter and it will not have parentheses
        if (ctx == SinglePar)
          abstractionResults map { brackets(_:Document) }
        else 
          abstractionResults
    }
  }
  
  /**
   * transform a scala type into an appropriate document
   * @param scalaType scala type to transform
   * @return an appropriate document
   */
  private def transform(scalaType: Scala.ScalaType): Document =
    scalaType match {
	  case Scala.Function(params, returnType) =>
	    paren(seqToDoc(params, ",", { param:Scala.ScalaType => transform(param) } )) :/:
	    "=>" :/: transform(returnType)	    		
	  case Scala.Const(name) => name	    		
	  case Scala.Instance(name, list) =>
	    name :: sqBrackets( seqToDoc(list, ",", transform(_:Scala.ScalaType) ) )
	  case _ => throw new RuntimeException
  	}
  
  /**
   * helper method which transforms each element of the given list
   * @param nodeList parameter list to transform
   * @return a list of documents, concatenation of lists of transformed documents
   * for each element of the parameter list 
   */
  private def transform(nodeList: List[Node], ctx: TransformContext): List[Document] = {
    (List[Document]() /: nodeList) {
      (list, node) => {
        list ++ transform(node, ctx)
      }
    }
  }
  
  /**
   * generates all documents which represent all combinations of parameters according
   * to the given parameter list
   * @param params parameter list for transform
   * @return list of documents with all parameter combinations
   */
  private def getParamsCombinations(params: List[Set[Node]], ctx: TransformContext = Par):List[Document] = {
    def getParamsCombinationsRec(listOfPicked: List[Document], params: List[Set[Node]]):List[Document] = {
      params match {
        case List() =>
          List(foldDoc(listOfPicked.tail, ","))
        case set :: list =>
          (List[Document]() /: transform(set.toList, ctx)) {
            (listSoFar, el) => {
              listSoFar ++ getParamsCombinationsRec(listOfPicked :+ el, list)
            }
          }
      }
    }
    
    getParamsCombinationsRec(List(empty), params)
  }
  
  /**
   * generates all documents which represent all combinations of parameters according
   * to the given parameter list and paramsInfo (for curring)
   * @param params parameter list for transform
   * @param paramsInfo parameter list information
   * @return list of documents with all parameter combinations
   */
  private def getParamsCombinations(params: List[Set[Node]], paramsInfo: List[List[Scala.ScalaType]], parenthesesRequired: Boolean):List[Document] = {
    def getParamsCombinationsRec(
        params: List[Set[Node]],
        paramsInfo: List[List[Scala.ScalaType]]):List[Document] = 
    {
      paramsInfo match {
        case List(lastList) =>
          assert(1 == paramsInfo.size)
          assert(params.size == lastList.size)
          // return the list of transformed last parentheses parameters
          getParamsCombinations(params)
        case currentList :: restOfTheList => {
          val currentListDocuments = getParamsCombinations(params take currentList.size)
          // go through all recursively got documents
          (List[Document]() /: getParamsCombinationsRec((params drop currentList.size), restOfTheList)) {
            (list, currentDocument) =>
              // add the combination with current parentheses documents
              list ++ currentListDocuments map {
                paren(_:Document) ::
                // if rest of the list is just one element it will be returned with no parentheses
                { if (restOfTheList.size == 1) currentDocument else paren(currentDocument) }
              }
          }
        }
        case Nil => List(empty)
      }
    }
    
    // if there is only one parameter and parentheses will not be outputed we have
    // to transform (potential) abstractions with braces
    if (params.size == 1 && !parenthesesRequired)
      getParamsCombinations(params, SinglePar)
    else
	  getParamsCombinationsRec(params, paramsInfo)
  }
  
  // ternary operator support
  case class Bool(b: Boolean) {
    def ?[X](t: => X) = new { 
      def |(f: => X) = if(b) t else f
    }
  }
	
  object Bool {
    implicit def BooleanBool(b: Boolean) = Bool(b)
  }
  
}

/**
 * class for encapsulation of code snippets
 */
case class CodeGenOutput(doc: Document) extends Formatable {
  def toDocument = doc
  // weight
}