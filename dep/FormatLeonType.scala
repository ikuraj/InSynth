package insynth.util.format

import scala.text.Document._
    
import leon.purescala.TypeTrees.{ TypeTree => LeonType }

case class FormatLeonType(tpe: LeonType) extends Formatable {
  override def toDocument = toDocument(tpe)
  
  def toDocument(tpe: LeonType): scala.text.Document = {
    import FormatHelpers._
    
//    tpe match {
//      case Method(receiver, params, returnType) =>
//        "Method" :: paren(
//            foldDoc(
//              for (list <- params) yield {
//                paren(seqToDoc(list, ",", toDocument(_:ScalaType)))
//              }, " "
//            )
//        )
//      case Function(params, returnType) =>
//        "Function" :: paren(
//            seqToDoc(params, ",", toDocument(_:ScalaType))
//        )   
//      case Const(name) =>
//        name
//      case Instance(name, list) => "Instance " :: name  
//      case Inheritance(subType, superType) => "Inheritance:" :: toDocument(subType) :: "-" :: toDocument(superType)
//      case null => "Null"
//      case _ => "Not implemented yet!"
//    }
    
    tpe.toString
  }
}