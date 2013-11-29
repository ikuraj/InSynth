package insynth
package attrgrammar

import org.scalatest._

import insynth.reconstruction.stream.Application
import insynth.common._
import insynth.reconstruction.stream._
import util.format._
import insynth.streams.{ ordered => ord }

class StreamablesTest extends FunSuite {

  import StreamableAST._
  import CommonLambda._
  
  case object Type
  
  case class Program(classes: Seq[Class])
  case class Class(methods: Seq[Method])
  case class Method(info: Int)
//  case class Method(statements: Seq[Statement])
  
  val streamFactory = new OrderedStreamFactory[Any]
  
  test("simpleConstruction") {
    
    case class Program(classes: Class)
    case class Class(methods: Method)
    case class Method()
    
    val methodNode = Empty
    val classNode = Single(classOf[Class], methodNode)
    val programNode = Single(classOf[Program], classNode)
    
    val streamables = new StreamablesIml(streamFactory)
    
    val resultStream = streamables.getStream(
      programNode,
      {
        case (clazz, c: Class) if clazz == classOf[Program] => Program(c)
        case (clazz, m: Method) if clazz == classOf[Class] => Class(m)
      },
      null,
      null
    )

    resultStream match {
      case us: ord.UnaryStream[_, _] =>
        us.streamable match {
          case us: ord.UnaryStream[_, _] =>
            us.streamable match {
              case ord.Empty =>                
              case _ => fail          
            }            
          case _ => fail          
        }
      case _ => fail
    }
      
  }

  test("injecter stream simple test") {
    
    case class Program(classes: Class)
    case class Class(methods: Method)
    case class Method(m: Int)
    
    val methodNode = Injecter(classOf[Method])
    val classNode = Single(classOf[Class], methodNode)
    val programNode = Single(classOf[Program], classNode)
    
    val streamables = new StreamablesIml(streamFactory)
    
    val methodStream = Stream( Method(1), Method(2), Method(3) )
    
    val resultStream = streamables.getStream(
      programNode,
      {
        case (clazz, c: Class) if clazz == classOf[Program] => Program(c)
        case (clazz, m: Method) if clazz == classOf[Class] => Class(m)
      },
      null,
      Map( classOf[Method] -> ( methodStream, false ) )
    )

    resultStream match {
      case us: ord.UnaryStream[_, _] =>
        us.streamable match {
          case us: ord.UnaryStream[_, _] =>
            us.streamable match {
              case ss: ord.SingleStream[_] => 
                assert( ss.getStream.toList == methodStream.toList) 
              case _ => fail          
            }            
          case _ => fail          
        }
      case _ => fail
    }
    
    expectResult( methodStream.map(m => Program(Class(m))) ) {
      resultStream.getStream
    }    
      
  }
  
//  @Test
//  def simpleConstruction {
//    
//    
//    val intNode = Injectable(classOf[Int])
//    val methodNode = Single(classOf[Method], intNode)
//    val methodCombiner = Single(classOf)
//    val classNode = Single(classOf[Class], methodNode)
//    val programNode = Single(classOf[Program], classNode)
//      
//  }

}

//object ReconstructorTest {
//  
//  var useEnumerationOrdering: Boolean = _
//  
//  @BeforeClass
//  def saveFlag = {
//    useEnumerationOrdering = Config.useEnumerationOrdering
//    Config.useEnumerationOrdering = false
//  }
//  
//  @AfterClass
//  def restoreFlag = Config.useEnumerationOrdering = useEnumerationOrdering
//  
//}
