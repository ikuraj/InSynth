package insynth.reconstruction.eager

import scala.collection.mutable.{ Map => MutableMap }
import scala.collection.mutable.{ Set => MutableSet }

import insynth.structures.{ SuccinctType => InSynthType, _ }

import insynth.common.CommonDomainTypes
import insynth.testdomain._

import scala.language.implicitConversions

object TreeExample {
	val fullNameClassA = "some.package.A"

	import CommonDomainTypes._
	
	private implicit def parameterToList(t: DomainType): List[DomainType] = List(t)
	private implicit def parameterToList(t: List[DomainType]): List[List[DomainType]] = List(t)
	private implicit def declarationToList(t: TestDeclaration): List[TestDeclaration] = List(t)
	
	private implicit def domainTypeToInSynthType(t: DomainType): InSynthType = t.toSuccinctType
	private def transform(t: DomainType): InSynthType = t.toSuccinctType
	
	// this **** can't work....
	type NodeMap = scala.collection.mutable.Map[DomainType, Node]

	def main(args: Array[String]): Unit = {
	  buildSimpleTree
	}

	def Declaration(name: String, s: InSynthType, d: DomainType) = TestDeclaration(d, s, name)
	
	def Declaration(d: DomainType) = TestDeclaration.newAbstract(d)
	
  //************************************
  // Scala types
  //************************************
  // class A { ... }
  val objectA = Atom(Const("A"))
  // def m4(): String	  
  val m4 = Function(List(objectA), typeString)
  // query: String → ⊥
  val queryType = Function(typeString, typeBottom)
  
  // NOTE InSynth query type: Arrow(TSet(List(Const(String))),Const($Bottom_Type_Just_For_Resolution$))
  
  //************************************
  // Declarations
  //************************************
  val objectADeclaration = Declaration(
      "some.package.A", // full name
      transform(objectA), // inSynth type
      objectA // scala type
    )
  
  val m4Declaration = Declaration(
      "some.package.A.m4", // full name
      transform(m4), // inSynth type
      m4 // scala type
    )		
  
  // special query declaration
  val queryDeclaration = Declaration(
      "special.name.for.query",
      transform(queryType),
      queryType
    )

	/**
	 * Constructs a simple tree (only one trivial method application).
	 * Based on the example we had when discussing.
	 */
	def buildSimpleTree = {
	  //************************************
	  // Goals
	  //	find expression of type: String
	  //	expression: query(m4(this))
	  //************************************
	  
	  //************************************
	  // InSynth proof trees
	  //************************************
	  
	  // XXX Unit→String is not the same as ()→String
	  // goal:String, type:A→String
	  // expression: m4(this):String
	  val getStringNode = SimpleNode(
	    m4Declaration,
	    MutableMap(
          // I will get object of class A from
          transform(objectA) ->
	  	  ContainerNode(
	  		  MutableSet(
	  		      SimpleNode(
  		    		  objectADeclaration,
  		    		  MutableMap() // this is the end, no further nodes
  		          )
  		      )
	        )
	      )
	    )
	  
      // goal:Bottom, type:String→⊥
      // expression: query(m4(this, Unit)):⊥
	  val query = 
	    SimpleNode(
	  	  queryDeclaration,
	  	  MutableMap( // for each parameter type - how can we resolve it
	  	      Const("String") ->
	  	      ContainerNode(
	  	          MutableSet(getStringNode)
	            )
	        ) 
	    )
	    
	  query
	}

	/**
	 * Constructs a complex tree.
	 * Based on the example we had when discussing.
	 */
	def buildComplexTree = {
	//***************************************************
	// Goals
	//	find expression of type: Boolean
	//	expression: query(m1(this, m2(this), m4(this)))
	//	code:
	// 	class A {
	//  	def m1(f: Int=>String, c:Char): Boolean
	//  	def m2(a: Int): String
	//  	def m3(a: Long): String
	//  	def m4(): Char
	//  	def m5(a: Int): Long
	//  	def m6(): String
	//  	def test() {
	//    		val b:Bool = ?synthesize?
	//  	}
	//	}
	//***************************************************
	  
	  //************************************
	  // Scala types
	  //************************************
	  // class A { ... }
	  val objectA = Atom(Const("A"))	
	  // def m1(f: Int=>String, c:Char): Boolean
	  val m1 = Function(
	      List ( objectA, Function(typeInt, typeString), typeChar ), // parameters
	      typeBoolean // return type
		)	
	  // def m2(a: Int): String 
	  val m2 = Function( List(objectA, typeInt), typeString)
	  // def m3(a:Long): String
	  val m3 = Function( List(objectA, typeLong), typeString)
	  // def m4(): Char
	  val m4 = Function( List(objectA), typeChar)
	  // def m5(a: Int): Long
	  val m5 = Function( List(objectA, typeInt), typeLong)
	  // def m6(): String
	  val m6 = Function( List(objectA), typeString)
	  // query: typeBoolean → ⊥
	  val queryType = Function(typeBoolean, typeBottom)
	  
	  // NOTE InSynth query type: Arrow(TSet(List(Const(String))),Const($Bottom_Type_Just_For_Resolution$))
	  
	  //************************************
	  // Declarations
	  //************************************
	  val objectADeclaration = Declaration(
	      fullNameClassA, // full name
	      transform(objectA), // inSynth type
	      objectA // scala type
	  )
	  
	  val m1Declaration	= Declaration(
	      fullNameClassA + ".m1",
	      transform(m1),
	      m1
	  )
	  val m2Declaration = Declaration(
	      fullNameClassA + ".m2", // full name
	      m2, // inSynth type (implicit conversion)
	      m2 // scala type
	  )
	  val m3Declaration = Declaration(
	      fullNameClassA + ".m3", // full name
	      m3, m3
      )
	  val m4Declaration = Declaration(
	      fullNameClassA + ".m4", // full name
	      m4, m4
      )
	  val m5Declaration = Declaration(
	      fullNameClassA + ".m5", // full name
	      m5, m5
      )
	  val m6Declaration = Declaration(
	      fullNameClassA + ".m6", // full name
	      m6, m6
      )		
	  
	  // special query declaration
	  val queryDeclaration = Declaration(
	      "special.name.for.query",
	      queryType, queryType
	    )	  
	  
	  //************************************
	  // InSynth proof trees
	  //************************************
	  
	  // XXX found out that there is a non-needed redundancy, ContainerNode type
	  // is actually not needed?
	  
	  // goal:ClassA object, type:ClassA
	  // expression: this	  
	  val thisNode = SimpleNode(
	      objectADeclaration, MutableMap()
      )
	    
	  // goal:Char, type:Unit→Char
	  // expression: m4(this)	  
	  val m4Node = SimpleNode(
	      m4Declaration,
	      MutableMap(
	          transform(objectA) -> ContainerNode(MutableSet(thisNode))
          )
      )
      
      // goal:(Int→String), type:(Int→String)
	  // expression: m2(this)
	  val m2Node = SimpleNode(
	      m2Declaration,
	      MutableMap(
	          transform(objectA) -> ContainerNode(MutableSet(thisNode)),
	          transform(typeInt) ->
	          	ContainerNode(MutableSet(SimpleNode(
	          	    { 
	          	      val dec = TestDeclaration(typeInt); dec	          	    	
	          	    }, MutableMap.empty
          	    )))
          )
      )      
      
      // goal:String, type:(A→String)
	  // expression: m6(this)
	  val m6Node = SimpleNode(
	      m6Declaration,
	      MutableMap(
	          transform(objectA) -> ContainerNode(MutableSet(thisNode))
          )
      )
            
      // goal: Long, type:(Int→Long)
	  // expression: m5(this, _)
	  val m5Node = SimpleNode(
	      m5Declaration,
	      MutableMap(
	          transform(objectA) -> ContainerNode(MutableSet(thisNode)),
	          transform(typeInt) -> ContainerNode( 
	          	MutableSet( SimpleNode(
	          	    { 
	          	      val dec = TestDeclaration(typeInt); dec	          	    	
	          	    }, MutableMap.empty
          	    ) )
	          )
          )
      )
      
      // goal:(Int→String), type:(Long→String)
	  // expression: Int => m3(this, m5(this, _))
	  val composeNode = SimpleNode(
	      m3Declaration,
	      MutableMap(
	          transform(objectA) -> ContainerNode(MutableSet(thisNode)),
	          transform(typeLong) -> ContainerNode(MutableSet(m5Node))
          )
      )
	    
	  // goal:Boolean, type:List((Int→String),Char)→Boolean
	  // expression: m1(this, 
      //				m2(this) |  m3(this) ∘ m5(this) | Int→m6(this), 
	  //				m4(this))	  
	  val m1Node = SimpleNode(
	      m1Declaration,
	      MutableMap(
	          transform(typeChar) -> ContainerNode(MutableSet(m4Node)),
	          transform(Function(typeInt, typeString)) ->
	          	ContainerNode( 
	          	    MutableSet(composeNode, m2Node, m6Node)
          	    ),
	          transform(objectA) -> ContainerNode(MutableSet(thisNode))
          )
      )
	  
      // goal:⊥, type:Boolean→⊥	    
      // expression: query(		m1(this,
	  //			m2(this) |  m3(this) ∘ m5(this) | Int→m6(this), 
	  //			m4(this)	)):⊥
	  val queryNode = 
	    SimpleNode(
	  	  queryDeclaration,
	  	  MutableMap( // for each parameter type - how can we resolve it
	  	      transform(typeBoolean) ->
	  	      ContainerNode(
	  	          MutableSet(m1Node)
	            )
	        ) 
	    )
	    
	  queryNode
	}
	
	/**
	 * Constructs a tree with an arrow goal type.
	 */
	def buildTreeArrowType = {
	//***************************************************
	// Goals
	//	find expression of type: (Int, Int)→Char
	//	expression:
	//	code:
	//  def outside(a: Int, b:Int): Char
	// 	class A {
	//		val intVal: Int  
	//  	def m1(): ((Int, Int)=>Char)
	//  	def m2(a: Int, b:Int): Char
	//  	def m3(): Char
	//  	def test() {
	//    		val b:(Int, Int)=>Char = ?synthesize?
	//  	}
	//	}
	//***************************************************
	  
	  //************************************
	  // Scala types
	  //************************************
	  // class A { ... }
	  val objectA = Atom(Const("A"))	
	  // def m1(): ((Int, Int)=>Char)
	  val m1 = Function(
	      List(objectA), // parameters
	      Function(List(typeInt, typeInt), typeChar) // return type
		)	
	  // def m2(a: Int, b:Int): Char
	  val m2 = Function( List(objectA, typeInt, typeInt), typeChar)
	  // def m3(): Char
	  val m3 = Function(List(objectA), typeChar)
	  // query: String → ⊥
	  val queryType = Function(
	    Function(List(typeInt, typeInt), typeChar),
	    typeBottom
	  )
	  // def outside(a: Int, b:Int): Char
	  val outside = Function(List(typeInt, typeInt), typeChar)
	  // val intVal: Int
	  val intVal = typeInt
	  	  	  
	  //************************************
	  // Declarations
	  //************************************
	  val objectADeclaration = Declaration(
	      fullNameClassA, // full name
	      transform(objectA), // inSynth type
	      objectA // scala type
	    )  
	  
	  val m1Declaration	= Declaration(
	      fullNameClassA + ".m1",
	      // XXX
	      m1,
	      //Arrow(TMutableSet(objectA), Function(List(typeInt, typeInt), typeChar)),
	      m1
	  )
	  val m2Declaration = Declaration(
	      fullNameClassA + ".m2", // full name
	      m2, // inSynth type (implicit conversion)
	      m2 // scala type
	  )
	  val m3Declaration = Declaration(
	      fullNameClassA + ".m3", // full name
	      m3, m3
      )
	  
	  // special query declaration
	  val queryDeclaration = Declaration(
	      "special.name.for.query",
	      queryType, queryType
	    )	  	
	  
	  val outsideDeclaration = Declaration(
	      "outside",
	      outside, outside
      )	 
	  val intValDeclaration = Declaration(
	      "A.intVal",
	      intVal, intVal
      )	 
            
	  val leafIntDeclaration = Declaration(typeInt)
	  
	  //************************************
	  // InSynth proof trees
	  //************************************	  
	  
	  // goal:A, type: A
	  // expression: d.fullname
	  val thisNode = SimpleNode(
	      objectADeclaration,
	      MutableMap()
      )
      
      // goal:Int, type:Int
	  // expression: A.intVal	  
	  val intValNode = SimpleNode(
	      intValDeclaration,
	      MutableMap()
      )
	  
	  // goal:(Int→Char), type:A→Int→Char
	  // expression: (Int,Int) → m1(this)(_, _)	  
	  val m1Node = SimpleNode(
	      m1Declaration,
	      MutableMap(
	          transform(objectA) -> ContainerNode(MutableSet(thisNode)),
	          transform(typeInt) -> ContainerNode(
	              MutableSet(SimpleNode(leafIntDeclaration, MutableMap.empty), intValNode))
          )
      )
      
      // goal:(Int→Char), type:(Int→Char)
	  // expression: d.fullName ("outside")	  
	  val outsideNode = SimpleNode(
	      outsideDeclaration,
	      MutableMap(
	          transform(typeInt) -> ContainerNode(
	              MutableSet(SimpleNode(leafIntDeclaration, MutableMap.empty), intValNode))
          )
      )
      
      // goal:(Char), type:(A→Char)
	  // expression: (Int,Int)→m3(A)	  
	  val m3Node = SimpleNode(
	      m3Declaration,
	      MutableMap(
	        transform(objectA) -> 
	          ContainerNode(MutableSet(thisNode))
          )
      )
      
      // goal:(Int→Char), type:((Int,A)→Char)
	  // expression: (Int, Int) → m2(this, _, _)	  
	  val m2Node = SimpleNode(
	      m2Declaration,
	      MutableMap(
	        transform(typeInt) -> 
        	  ContainerNode(MutableSet(SimpleNode(leafIntDeclaration, MutableMap.empty), intValNode)),
	        transform(objectA) ->
        	  ContainerNode(MutableSet(thisNode))
          )
      )     
	  
      // goal:⊥, type:(Int→Char)→⊥	    
      // expression: query	(		
      //	(Int,Int) -> m1(this)(_,_) | (Int,Int) -> m1(this)(intVal, intVal)
	  //	(Int,Int) -> m2(this,_,_) | m2(this, intVal, intVal)
      //	(Int,Int) -> m3(this) | outside
      //					):⊥
	  val queryNode = 
	    SimpleNode(
	  	  queryDeclaration,
	  	  MutableMap( // for each parameter type - how can we resolve it
	  	      transform(Function(List(typeInt, typeInt), typeChar)) ->
	  	      ContainerNode(
	  	          MutableSet(m1Node, outsideNode, m2Node, m3Node)
	            )
	        ) 
	    )
      queryNode
	}
	
	/**
	 * Constructs a tree in which expression can be synthesized as a return one
	 * but also as a parameter to one of the methods
	 */
	def buildTreeOverlapParameterTypeWithReturnType = {
	//***************************************************
	// Goals
	//	find expression of type: (Int, Int)→Char
	//	expression:
	//	code:
	//  def outside(a: Int, b:Int): Char
	// 	class A {
	//		val intVal: Int  
	//  	def m1(): ((Int, Int)=>Char)
	//  	def m2(a: Int, b:Int): Char
	//  	def m3(): Char
	//  	def test() {
	//    		val b:(Int, Int)=>Char = ?synthesize?
	//  	}
	//	}
	//***************************************************
	  
	  //************************************
	  // Scala types
	  //************************************
	  // class A { ... }
	  val objectA = Atom(Const("A"))	
	  // def m1(): ((Int, Int)=>Char)
	  val m1 = Function(
	      List(objectA), // parameters
	      Function(List(typeInt, typeInt), typeChar) // return type
		)	
	  // def m2(a: Int, b:Int): Char
	  val m2 = Function(List(objectA, typeInt, typeInt), typeChar)
	  // def m3(): Char
	  val m3 = Function(List(objectA), typeChar)
	  // query: String → ⊥
	  val queryType = Function(
	    Function(List(typeInt, typeInt), typeChar),
	    typeBottom
	  )
	  // def outside(a: Int, b:Int): Char
	  val outside = Function(List(typeInt, typeInt), typeChar)
	  // val intVal: Int
	  val intVal = typeInt
	  	  	  
	  //************************************
	  // Declarations
	  //************************************
	  val objectADeclaration = Declaration(
	      fullNameClassA, // full name
	      transform(objectA), // inSynth type
	      objectA // scala type
	    )
	  
	  val m1Declaration	= Declaration(
	      fullNameClassA + ".m1",
	      // XXX
	      m1,
	      //Arrow(TSet(objectA), Function(List(typeInt, typeInt), typeChar)),
	      m1
	  )
	  val m2Declaration = Declaration(
	      fullNameClassA + ".m2", // full name
	      m2, // inSynth type (implicit conversion)
	      m2 // scala type
	  )
	  val m3Declaration = Declaration(
	      fullNameClassA + ".m3", // full name
	      m3, m3
      )
	  
	  // special query declaration
	  val queryDeclaration = Declaration(
	      "special.name.for.query",
	      queryType, queryType
	    )	  	
	  
	  val outsideDeclaration = Declaration(
	      "outside",
	      outside, outside
      )	 
	  val intValDeclaration = Declaration(
	      "A.intVal",
	      intVal, intVal
      )	 
      
	  val leafIntDeclaration = Declaration(typeInt)
	  
	  //************************************
	  // InSynth proof trees
	  //************************************	  
	  
	  // goal:A, type: A
	  // expression: d.fullname
	  val thisNode = SimpleNode(
	      objectADeclaration,
	      MutableMap()
      )
      
      // goal:Int, type:Int
	  // expression: A.intVal	  
	  val intValNode = SimpleNode(
	      intValDeclaration,
	      MutableMap()
      )
	  
	  // goal:(Int→Char), type:A→Int→Char
	  // expression: (Int,Int) → m1(this)(_, _)	  
	  val m1Node = SimpleNode(
	      m1Declaration,
	      MutableMap(
	          transform(objectA) -> ContainerNode(MutableSet(thisNode)),
	          transform(typeInt) -> ContainerNode(
	              MutableSet(SimpleNode(leafIntDeclaration, MutableMap.empty), intValNode))
          )
      )
	  
      // goal:(Int→Char), type:((Int,A)→Char)
	  // expression: (Int, Int) → m2(this, _, _)	  
	  val m2Node = SimpleNode(
	      m2Declaration,
	      MutableMap(
	        transform(typeInt) -> 
        	  ContainerNode(MutableSet(SimpleNode(leafIntDeclaration, MutableMap.empty), intValNode)),
	        transform(objectA) ->
        	  ContainerNode(MutableSet(thisNode))
          )
      )     
      
      // goal:(Int→Char), type:(Int→Char)
	  // expression: d.fullName ("outside")	  
	  val outsideNode = SimpleNode(
	      outsideDeclaration,
	      MutableMap(
	          transform(typeInt) -> ContainerNode(
	              MutableSet(SimpleNode(leafIntDeclaration, MutableMap.empty), intValNode))
          )
      )
      
      // goal:(Char), type:(A→Char)
	  // expression: (Int,Int)→m3(A)	  
	  val m3Node = SimpleNode(
	      m3Declaration,
	      MutableMap(
	        transform(objectA) -> 
	          ContainerNode(MutableSet(thisNode))
          )
      )
      	  
      // goal:⊥, type:(Int→Char)→⊥	    
      // expression: query	(		
      //	(Int,Int) -> m1(this)(_,_) | (Int,Int) -> m1(this)(intVal, intVal)
	  //	(Int,Int) -> m2(this,_,_) | m2(this, intVal, intVal)
      //	(Int,Int) -> m3(this) | outside
      //					):⊥
	  val queryNode = 
	    SimpleNode(
	  	  queryDeclaration,
	  	  MutableMap( // for each parameter type - how can we resolve it
	  	      transform(Function(List(typeInt, typeInt), typeChar)) ->
	  	      ContainerNode(
	  	          MutableSet(m1Node, outsideNode, m2Node, m3Node)
    		  )
	        ) 
	    )
      queryNode
	}	
	
	/**
	 * Small example that uses function parameter application
	 */
	def buildTreeAbsApplication = {
	//***************************************************
	// Goals
	//	find expression of type: (Int→Char, Int)→Char
	//	expression:
	//	code:
	//  	def test() {
	//    		val b:(Int→Char)→Char = ?synthesize?
	//  	}
	//***************************************************
	  
	  //************************************
	  // Scala types
	  //************************************
	  
	  // query: (Int→Char)→Char → ⊥
	  val queryType = Function(
	    Function(List(Function(typeInt, typeChar), typeInt), typeChar),
	    typeBottom
	  )
	  
	  val intLeafNode = SimpleNode(Declaration(typeInt), MutableMap.empty)
	  	  	  
	  //************************************
	  // Declarations
	  //************************************
	 	  
	  // special query declaration
	  val queryDeclaration = Declaration(
	      "special.name.for.query",
	      queryType, queryType
	    )	  
	  
	  //************************************
	  // InSynth proof trees
	  //************************************	
      
      // goal:(Int→Char, Int)→Char, type:Char
	  // expression: (Int, Int) → m2(this, _, _)	  
	  val absNode = SimpleNode(
	      Declaration(Function(typeInt, typeChar)),
	      MutableMap(
	        transform(typeInt) -> 
        	  ContainerNode(MutableSet(intLeafNode))
          )
      )
	  
      // goal:⊥, type:(Int→Char, Int)→Char→⊥	    
      // expression: query	(		
      //	(Int→Char, Int) -> _(_)
      //					):⊥
	  val queryNode = 
	    SimpleNode(
	  	  queryDeclaration,
	  	  MutableMap( // for each parameter type - how can we resolve it
	  	      transform(Function(List(Function(typeInt, typeChar), typeInt), typeChar)) ->
	  	      ContainerNode(
	  	          MutableSet(absNode)
	            )
	        ) 
	    )
      queryNode
	}	
	
	/**
	 * Small example that uses function parameter application
	 */
	def buildTreeSKombinator = {
	//***************************************************
	// Goals
	//	find expression of type: (Int→(Char→String)) → (Int→Char) → Int→String
	//	expression:
	//	code:
	//  	def test() {
	//    		val b:(Int=>(Char=>String))=>(Int=>Char)=>Int=>String = ?synthesize?
	//  	}
	//***************************************************
	  
	  //************************************
	  // Scala types
	  //************************************
	  
	  val sKombType = 
	    Function(
          List(Function(typeInt, Function(typeChar, typeString))),
          Function(
            Function(typeInt, typeChar),
            Function(typeInt, typeString)
          )
		)
	  
	  // query: (Int→(Char→String)) → (Int→Char) → Int→String → ⊥
	  val queryType = Function(
	    sKombType,
	    typeBottom
	  )
	  	  	  
	  //************************************
	  // Declarations
	  //************************************
	 	  
	  // special query declaration
	  val queryDeclaration = Declaration(
	      "special.name.for.query",
	      queryType, queryType
	    )	  
	  
	  //************************************
	  // InSynth proof trees
	  //************************************	
      
	  val intLeafNode = SimpleNode(Declaration(typeInt), MutableMap.empty)
	  
      // TODO
      // goal:(Int→Char, Int)→Char, type:Char
	  // expression: (Int, Int) → m2(this, _, _)	????  
	  val absNode2 = SimpleNode(
	      Declaration(Function(List(typeInt), typeChar)),
	      MutableMap(
	        transform(typeInt) -> 
        	  ContainerNode(MutableSet(intLeafNode))     	  
          )
      )     
	    
      // TODO
      // goal:(Int→Char, Int)→Char, type:Char
	  // expression: (Int, Int) → m2(this, _, _)	????  
	  val absNode1 = SimpleNode(
	      Declaration(Function(typeInt, Function(typeChar, typeString))),
	      MutableMap(
	        transform(typeInt) -> 
        	  ContainerNode(MutableSet(intLeafNode)),
        	transform(typeChar) ->
        	  ContainerNode(MutableSet(absNode2))        	  
          )
      )     

      // TODO      
      // goal:⊥, type:(Int→Char, Int)→Char→⊥	    
      // expression: query	(		
      //	(Int→Char, Int) -> _(_)
      //					):⊥????
	  val queryNode = 
	    SimpleNode(
	  	  queryDeclaration,
	  	  MutableMap( // for each parameter type - how can we resolve it
	  	      transform(sKombType) ->
	  	      ContainerNode(
	  	          MutableSet(absNode1)
	            )
	        ) 
	    )
      queryNode
	}	
	
	
	/**
	 * Constructs a simple tree which has calls to various variants of application
	 * (method, function, constructor)
	 */
	def buildTreeWithVariousFunctions = {
	  //************************************
	  // Goals
	  //	find expression of type: String
	  //	expression: query((this.m(), this.bla))
	  //************************************
	  
	  //************************************
	  // Scala types
	  //************************************
	  // class A { ... }
	  val objectA = Atom(Const("A"))		
	  // class B { ... }
	  val objectB = Atom(Const("B"))	
	  // constructor B(String, Int)
	  val constructB = Function(List(typeString, typeInt), objectB)
	  // def m(): String	  
	  val m = Function(List(objectA), typeString)
	  // int field
	  val intField = Function(List(objectA), typeInt)
	  // query: String → ⊥
	  val queryType = Function(objectB, typeBottom)
	  
	  // NOTE InSynth query type:
	  // Arrow(TSet(List(Const(B))),Const($Bottom_Type_Just_For_Resolution$))
	  
	  //************************************
	  // Declarations
	  //************************************
	  val objectADeclaration = Declaration(
	      "obj", // full name
	      transform(objectA), // inSynth type
	      objectA // scala type
	    )
	  //objectADeclaration.setIsThis(true)
	  // needs a constructor
	  
	  val mDeclaration = Declaration(
	      "some.package.A.m", // full name
	      transform(m), // inSynth type
	      m // scala type
	    )		
	  
	  val constructBDeclaration = Declaration(
	      "some.package.B.cst", constructB, constructB
      )
	  	  
	  val intValDeclaration = Declaration(
	      "A.intVal",
	      intField, intField
      )	 
	  
	  // special query declaration
	  val queryDeclaration = Declaration(
	      "special.name.for.query",
	      transform(queryType),
	      queryType
	    )	  
	  
	  //************************************
	  // InSynth proof trees
	  //************************************
	  	  
	  val thisNode = SimpleNode(
	      objectADeclaration,
	      MutableMap()
      )
	  
	  // goal:String, type:String, →B
	  // expression: (this.m(), this.bla)
	  val getBNode = SimpleNode(
	    constructBDeclaration,
	    MutableMap(
          // I will get object of class A from
          transform(typeString) ->
	  	  ContainerNode(
	  		  MutableSet(
	  		      SimpleNode(
  		    		  mDeclaration,
  		    		  MutableMap( transform(objectA) -> ContainerNode(MutableSet( thisNode )))
  		          )
  		      )
	        ),
          transform(typeInt) ->
	  	  ContainerNode(
	  		  MutableSet(
	  		      SimpleNode(
  		    		  intValDeclaration,
  		    		  MutableMap( transform(objectA) -> ContainerNode(MutableSet( thisNode )))  		    		  
  		          )
  		      )
	        )
	      )
	    )
	  
      // goal:Bottom, type:B→⊥
      // expression: query((this.m(), this.bla)):⊥
	  val query = 
	    SimpleNode(
	  	  queryDeclaration,
	  	  MutableMap( // for each parameter type - how can we resolve it
	  	      transform(objectB) ->
	  	      ContainerNode(
	  	          MutableSet(getBNode)
	            )
	        ) 
	    )
	    
	  query
	}
	
	/**
	 * Constructs a simple tree which has cycles
	 */
	def buildTreeCycles = {
	  //************************************
	  // Goals
	  //	find expression of type: Int
	  //	expression: query(intVal | f(intVal) | f(f(intVal)) | ... )
	  //************************************
	  	  
	  //************************************
	  // Scala types
	  //************************************
	  // def f(): Int=>Int	  
	  val f = Function(List(typeInt), typeInt)
	  // query: Int → ⊥
	  val queryType = Function(typeInt, typeBottom)
	  
	  // NOTE InSynth query type:
	  // Arrow(TMutableSet(List(typeInt)),Const($Bottom_Type_Just_For_Resolution$))
	  
	  //************************************
	  // Declarations
	  //************************************	  
	  val fDeclaration = Declaration(
	      "some.package.f", // full name
	      transform(f), // inSynth type
	      f // scala type
	    )		
	  	  	  
	  val intValDeclaration = Declaration(
	      "intVal",
	      typeInt, typeInt
      )	 
	  
	  // special query declaration
	  val queryDeclaration = Declaration(
	      "special.name.for.query",
	      transform(queryType),
	      queryType
	    )	  
	  
	  //************************************
	  // InSynth proof trees
	  //************************************
	  	        
      val intNode = SimpleNode(
          intValDeclaration,
          MutableMap()
      )
	  
	  val getIntNode:SimpleNode = SimpleNode(
	    fDeclaration,
	    MutableMap()
	  )
	  
	  getIntNode.getParams +=
	    (
          transform(typeInt) ->
	  	  ContainerNode(
	  		  MutableSet(intNode, getIntNode)
	        )
	    )
	  
//	  lazy val getIntNodeRec = SimpleNode(
//	    fDeclaration,
//	    MutableMap(
//          transform(typeInt) ->
//	  	  ContainerNode(
//	  		  MutableSet(getIntNode)
//	        )
//	    )
//	  )
	  	  
	  val query = 
	    SimpleNode(
	  	  queryDeclaration,
	  	  MutableMap( // for each parameter type - how can we resolve it
	  	      transform(typeInt) ->
	  	      ContainerNode(
	  	          MutableSet(getIntNode)
	            )
	        ) 
	    )
	    
	  query
	}
		
	def buildTreeIdentityFunction = {
	  //************************************
	  // Goals
	  //	find expression of type: Int=>Int
	  //	expression: query(x:Int => Int)
	  //************************************
	  
	  //************************************
	  // Scala types
	  //************************************
	  
	  val neededType = Function(typeInt, typeInt)
		
	  val queryType = Function(neededType, typeBottom)
	  
	  //************************************
	  // Declarations
	  //************************************
	  	  
	  // special query declaration
	  val queryDeclaration = Declaration(
	      "special.name.for.query",
	      transform(queryType),
	      queryType
	    )	  
	  
	  //************************************
	  // InSynth proof trees
	  //************************************
	  	  	  
	  val intLeafNode = SimpleNode(Declaration(typeInt), MutableMap.empty)

	  val absNode = SimpleNode(
	      Declaration(Function(typeInt, typeInt)),
	      MutableMap(
	        transform(typeInt) -> 
        	  ContainerNode(MutableSet(intLeafNode))      	  
          )
      )     
	  
	  val query = 
	    SimpleNode(
	  	  queryDeclaration,
	  	  MutableMap( // for each parameter type - how can we resolve it
	  	      transform(Function(typeInt, typeInt)) ->
	  	      ContainerNode(
	  	          MutableSet(intLeafNode)
	            )
	        ) 
	    )
	    
	  query
	}
	
	/**
	 * Constructs a simple tree which has two methods with same InSynth type but
	 * different number of parameters (and weights sum)
	 */
	def buildSameInSynthDifferentWeight = {
	  //************************************
	  // Goals
	  //	find expression of type: String
	  //	expression: query(f1(i), f2(i, i))
	  //************************************
	  
	  //************************************
	  // Scala types
	  //************************************
	  // def f1(Int): String	  
	  val f1 = Function(List(typeInt), typeString)
	  // def f2(Int, Int): String	  
	  val f2 = Function(List(typeInt, typeInt), typeString)
	  // query: String → ⊥
	  val queryType = Function(typeString, typeBottom)
	  
	  
	  //************************************
	  // Declarations
	  //************************************
	  	  
	  val f1Declaration = Declaration(
	      "f1", // full name
	      transform(f1), // inSynth type
	      f1 // scala type
	    )		
	  
	  val f2Declaration = Declaration(
	      "f2", // full name
	      transform(f2), // inSynth type
	      f2 // scala type
	    )		
	  
	  val intValDeclaration = Declaration(
	      "intVal",
	      typeInt, typeInt
      )	 
	  
	  // special query declaration
	  val queryDeclaration = Declaration(
	      "special.name.for.query",
	      transform(queryType),
	      queryType
	    )	  
	  
	  //************************************
	  // InSynth proof trees
	  //************************************
	  	        
	  val f1Node = SimpleNode(
	    f1Declaration,
	    MutableMap(
          transform(typeInt) ->
	  	  ContainerNode(
	  		  MutableSet(
	  		      SimpleNode(
  		    		  intValDeclaration,
  		    		  MutableMap()  		    		  
  		          )
  		      )
	        )
	      )
	    )
	  
	  
	  val f2Node = SimpleNode(
	    f2Declaration,
	    MutableMap(
          transform(typeInt) ->
	  	  ContainerNode(
	  		  MutableSet(
	  		      SimpleNode(
  		    		  intValDeclaration,
  		    		  MutableMap()  		    		  
  		          )
  		      )
	        )
	      )
	    )
	  
	  val query = 
	    SimpleNode(
	  	  queryDeclaration,
	  	  MutableMap(
	  	      transform(typeString) ->
	  	      ContainerNode(
	  	          MutableSet(f1Node, f2Node)
	            )
	        ) 
	    )
	    
	  query
	}
	
}