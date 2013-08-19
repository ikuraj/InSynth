package insynth.reconstruction.intermediate

object IntermediateTreeOperations {
	import insynth.reconstruction.intermediate._
	
	def size(n: Node): Int = size(n, Set())

	private def size(n: Node, visited: Set[Node]): Int = {
	  if (visited contains n) 
	  	0
  	else n match {
	    case _: Leaf | _: Variable | _: Identifier | NullLeaf =>
	      1
	    case Abstraction(_, _, params) =>
	      (0 /: params) {
        	(res, param) => res + size(param, visited + n)
      	}
	    case Application(_, params) =>
	      (for (appSet <- params)
	      	yield (0 /: appSet) {
	        	(res, app) => res + size(app, visited + n)
        	}).sum
	  }
	}
}