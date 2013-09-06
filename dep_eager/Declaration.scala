package insynth.reconstruction.eager.combinator

import insynth.load.{ Declaration => InSynthDeclaration }
import insynth.structures.{ _ }

object Declaration {
  
  val predefinedAbstractDeclarationWeight = 1.0f
  
  implicit def fromBaseDeclaration(dec: InSynthDeclaration): Declaration = {
    dec match {
      case _ if dec.isAbstract => AbsstractDeclaration(dec)
      case _ => NormalDeclaration(dec)
    }
  }  
}

import Declaration._

abstract class Declaration(val dec: InSynthDeclaration) {

  def getType = dec.getType
  
  def getDomainType = domainType

  def getWeight: Float
  def getSimpleName: String

}

case class AbsstractDeclaration(dec: InSynthDeclaration) extends Declaration(dec) {
    
  def getWeight = dec.getWeight

  def getSimpleName = "#abs#" + dec.getSimpleName
    
}

case class NormalDeclaration(val dec: InSynthDeclaration) extends Declaration(dec) {

  def getWeight = dec.getWeight

  def getSimpleName = dec.getSimpleName

}