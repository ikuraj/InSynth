package ch.epfl.insynth.reconstruction.codegen

import ch.epfl.insynth.reconstruction.intermediate._
import ch.epfl.insynth.trees
import ch.epfl.scala.{ trees => Scala }
import ch.epfl.insynth.print._
import ch.epfl.insynth.reconstruction.combinator.{ NormalDeclaration, AbsDeclaration }

import scala.text.Document
import scala.text.Document.empty

/** companion object for more convenient application */
object ClassicStyleCodeGenerator {
  def apply(tree: Node) = {
    (new ClassicStyleCodeGenerator).apply(tree)
  }
}

/**
 * this class support scala syntax without omitting any unnecessary parentheses and dots
 */
class ClassicStyleCodeGenerator extends CodeGenerator {
  // import methods for easier document manipulation
  import FormatHelpers._
  import Document._
  import TransformContext._
  
  /**
   * main method (recursive) for transforming a intermediate (sub)tree
   * @param tree root node of the (sub)tree to transform 
   * @return list of documents containing all combinations of available expression for
   * the given (sub)tree
   */
  override def transform(tree: Node, ctx: TransformContext = Expr): List[Document] = {
    // we want ternary operator
    import Bool._
    
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
        // (which tells us, whether it is a function, a method...)
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
                (list2, paramsDoc) =>
                  list2 :+
                  	// concatenate identifier and parameters in parentheses
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

            // go through all combinations of parameters documents
            return (List[Document]() /: getParamsCombinations(params.drop(2))) {
              (list, paramsDoc) =>
                list :+
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

          	// go through all combinations of parameters documents
          	return (List[Document]() /: getParamsCombinations(params.drop(2), paramsInfo, true)) {
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
              (listDocsReceivers, receiver) =>
                {
                  // get documents for the receiver objects (or empty if none is needed)
                  val documentsForThis = {
                    if (!needsThis)
                      receiver match {
                        case Identifier(_, NormalDeclaration(receiverDecl)) if receiverDecl.isThis =>
                          List(empty)
                        case _ => transform(receiver, App) map { (_: Document) :: "." }
                      }
                    else transform(receiver, App) map { (_: Document) :: "." }
                  }
                  // go through all the receiver objects and add to the list
                  (listDocsReceivers /: documentsForThis) {
                    (listDocsTransformedReceiver, receiverDoc) =>
                      {
                        listDocsTransformedReceiver :+ group(receiverDoc :: appIdentifier)
                      }
                  }
                }
            }
          } 
	          	        
	        else if (!decl.isMethod) {
	          assert(!decl.isConstructor)
	          
        	  // just a function
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

              // if the method needs this keyword
              val needsThis = decl.hasThis
              (List[Document]() /: params(1)) {
                (listDocsReceivers, receiver) =>
                  {
                    // get documents for the receiver objects (or empty if none is needed)
                    val documentsForThis = {
                      if (!needsThis)
                        receiver match {
                          case Identifier(_, NormalDeclaration(receiverDecl)) if receiverDecl.isThis =>
                            List(empty)
                          case _ => transform(receiver, App) // map { (_:Document) :: "." }			            
                        }
                      else transform(receiver, App) // map { (_:Document) :: "." }
                    }
                    // go through all the receiver objects and add to the list
                    (listDocsReceivers /: documentsForThis) {
                      (listDocsTransformedReceiver, receiverDoc) =>
                        {
                          // go through all combinations of parameters documents
                          (listDocsTransformedReceiver /: getParamsCombinations(params.drop(2), paramsInfo, true)) {
                            // and add them to the list
                            (listDocsTransformedParameters, paramsDoc) =>
                              listDocsTransformedParameters :+
                                group(doParenRecApp(receiverDoc, appIdentifier, paramsDoc))
                          }
                        }
                    }
                  }
              }
            }
          } // case Identifier
          
          // function that is created as an argument or anything else
          case Identifier(_, _:AbsDeclaration) | _:Variable | _ =>
          	firstTermFunctionTransform
          	
        } // params.head.head match 
      }
      
      // abstraction first creates all of its arguments
      case Abstraction(tpe, vars, subtrees) =>
        assert(vars.size > 0)
        
        // check if we need parentheses for variables
        val parenthesesRequired = vars.size > 1

        // for all bodies of this abstraction
        val abstractionResults = (List[Document]() /: subtrees) {
          (listOfAbstractions, body) =>
            {
              listOfAbstractions ++
                // for all transformations of bodies
                (List[Document]() /: transform(body)) {
                  (listOfBodies, transformedBody) =>
                    listOfBodies :+ (
                      // transform argument variables
                      parenthesesRequired ? 
                      		paren(seqToDoc(vars, ",", { v: Variable => transform(v, Arg).head })) |
                      		seqToDoc(vars, ",", { v: Variable => transform(v, Arg).head })
                      :/: "=>" :/:
                      // transform the body
                      transformedBody)
                }
            }
        }

        // return abstraction results
        // we need brackets only if this abstraction is parameter and it will not have parentheses
        if (ctx == SinglePar)
          abstractionResults map { brackets(_: Document) }
        else
          abstractionResults
    } // tree match
  }
   
  // helper method for application, output parentheses always
  def doParenApp(appId: Document, params: Document) = appId :: paren(params)
  
	// helper method
  def doParenRecApp(receiverDoc: Document, appId: Document, params: Document) = {
    // to match the empty document
    val emptyMatch = empty
    
    receiverDoc match {
      case `emptyMatch` =>
        appId :: paren(params) 
      case _:Document =>
        receiverDoc :: "." :: appId :: paren(params)
    }
  }
  
}