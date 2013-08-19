package insynth.structures

/**
 * DomainType is here so that early reconstruction phases know
 * the structure of types without the need to know the actual
 * type 
 */
sealed abstract class DomainType

case object Atom extends DomainType
case class Function(args: List[Atom]) extends DomainType