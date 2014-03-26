package insynth
package streams
package light
package sampling

import light._
import streams.{ dependent => dep }

trait Dependent[I, O] extends dep.Dependent[I, O] {

  override def getStream(parameter: I): SamplableEnum[O]
  
}