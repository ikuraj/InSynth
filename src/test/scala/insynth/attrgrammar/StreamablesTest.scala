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
  
//  test("simpleConstruction") {
//    
//    case class Program(classes: Class)
//    case class Class(methods: Method)
//    case class Method()
//    
//    val methodNode = Empty
//    val classNode = Single(classOf[Class], methodNode)
//    val programNode = Single(classOf[Program], classNode)
//    
//    val streamables = new StreamablesIml(streamFactory)
//    
//    val resultStream = streamables.getStreamable(
//      programNode,
//      {
//        case (clazz, c: Class) if clazz == classOf[Program] => Program(c)
//        case (clazz, m: Method) if clazz == classOf[Class] => Class(m)
//      },
//      null,
//      null
//    )
//
//    resultStream match {
//      case us: ord.UnaryStream[_, _] =>
//        us.streamable match {
//          case us: ord.UnaryStream[_, _] =>
//            us.streamable match {
//              case ord.Empty =>                
//              case _ => fail          
//            }            
//          case _ => fail          
//        }
//      case _ => fail
//    }
//      
//  }
//
//  test("injecter stream simple test") {
//    
//    case class Program(classes: Class)
//    case class Class(methods: Method)
//    case class Method(m: Int)
//    
//    val methodNode = Injecter(classOf[Method])
//    val classNode = Single(classOf[Class], methodNode)
//    val programNode = Single(classOf[Program], classNode)
//    
//    val streamables = new StreamablesIml(streamFactory)
//    
//    val methodStream = Stream( Method(1), Method(2), Method(3) )
//    
//    val resultStream = streamables.getStreamable(
//      programNode,
//      {
//        case (clazz, c: Class) if clazz == classOf[Program] => Program(c)
//        case (clazz, m: Method) if clazz == classOf[Class] => Class(m)
//      },
//      null,
//      Map( classOf[Method] -> ( methodStream, false ) )
//    )
//
//    resultStream match {
//      case us: ord.UnaryStream[_, _] =>
//        us.streamable match {
//          case us: ord.UnaryStream[_, _] =>
//            us.streamable match {
//              case ss: ord.FiniteStream[_] => 
//                assert( ss.getStream.toList == methodStream.toList) 
//              case _ => fail          
//            }            
//          case _ => fail          
//        }
//      case _ => fail
//    }
//    
//    expectResult( methodStream.map(m => Program(Class(m))) ) {
//      resultStream.getStream
//    }    
//      
//  }
  
  test("aggregator stream simple test") {
    
    case class Program(classes: Class)
    case class Class(methods: Seq[Method])
    case class Method(m: Int)
    
    val methodNode = Injecter(classOf[Method])
    val aggregatedMethodNode = Generator(classOf[Method], methodNode)
    val classNode = Combiner(classOf[Class], aggregatedMethodNode)
    val programNode = Single(classOf[Program], classNode)
    
    val streamables = new StreamablesIml(streamFactory)
    
    val methodStream = Stream( Method(1), Method(2), Method(3) )
    
    val resultStream = streamables.getStreamPairs(
      programNode,
      {
        case (clazz, m: Class) if clazz == classOf[Program] => Program(m)
      },
      {
        case (clazz, methodList: List[_]) if clazz == classOf[Class] =>
          Class(methodList.asInstanceOf[List[Method]])
      },
      Map( classOf[Method] -> ( methodStream, false ) )
    )

//    withClue(FormatStreamUtils(resultStream).toString) {
//      resultStream match {
//        case us: ord.UnaryStream[_, _] =>
//          us.streamable match {
//            case us: ord.LazyRoundRobbin[_] =>
//              us.streams match {
//                case (singleton: ord.Singleton[_]) ::
//                  (bs: ord.BinaryStream[_, _, _]) :: Nil => 
//                case _ => fail          
//              }            
//            case _ => fail          
//          }
//        case _ => fail
//      }
//    }
    
    val expected =
      List((Nil, 1.0), (List(Method(1)), 2.0), (List(Method(2)), 3.0), (List(Method(3)), 4.0),
        (List(Method(1), Method(1)), 3.0), (List(Method(1), Method(2)), 4.0)) map {
        case (list, w) => (Program(Class(list)), w + 1)
      }
        
    withClue ( resultStream.take(25).mkString("\n") ) {
      for(ex <- expected)
        assert( resultStream.take(25).toSet contains ex, "does not contain " + ex )
    }    
    
//    withClue (resultStream.getStream.take(5).mkString(",")) {
//      expectResult(
//        List(Nil, List(Method(1)), List(Method(2)), List(Method(3)), List(Method(3)),
//          List(Method(1), Method(1)), List(Method(1), Method(2)), List(Method(1), Method(3)),
//          List(Method(2), Method(1)), List(Method(2), Method(2)), List(Method(2), Method(3)),
//          List(Method(3), Method(1)), List(Method(3), Method(2)), List(Method(3), Method(3))
//      )) {
//        resultStream.getStream.take(5)
//      }
//    }    
      
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
