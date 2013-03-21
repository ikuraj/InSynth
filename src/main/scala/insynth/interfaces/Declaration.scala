package insynth.interfaces

import insynth.structures.{ SuccinctType => Type }
import insynth.structures.Weight._

abstract class Declaration(val inSynthType: Type, val weight: Weight) {        
	
  private var query = false
  
  def getWeight = weight  
  //def setWeight(weight:Weight) { this.weight = weight }
  
  def isQuery = this.query
  def setIsQuery(query:Boolean){ this.query = query }
      
  def getType = inSynthType
  
}