package insynth
package testdomain

import insynth.structures.SuccinctType
import insynth.structures.Weight._

case class TestDeclaration(
	override val inSynthType: SuccinctType, override val weight: Weight = 1.0f
)
extends insynth.load.Declaration(inSynthType, weight) {
  
  override def getSimpleName = inSynthType.toString
  
}