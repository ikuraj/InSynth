package insynth.interfaces

import insynth.engine.Sender
import insynth.structures.ContainerNode
import insynth.structures.SuccinctType

abstract class Query(val inSynthRetType: SuccinctType) {
  
  def getSolution: ContainerNode
  
  def getDeclaration: Declaration
  
  def getReturnType = inSynthRetType
  
  def getSender: Sender
  
}