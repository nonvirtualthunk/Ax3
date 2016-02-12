package arx.tmp.game.logic.descriptors

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/14
 * Time: 8:58 AM
 */

import arx.tmp.game.logic.entities.core.GameEntity

class ORDescriptor(val first:TDescriptor,val second:TDescriptor) extends TDescriptor {
	override protected def doesMatch(ref: Any): Boolean = first.matches(ref) || second.matches(ref)

	override def exampleMatch: Serializable = first.exampleMatch
}


class CompoundDescriptor protected (var subDescriptors : List[TDescriptor]) extends TDescriptor {
	override protected def doesMatch(ref: Any): Boolean = {
		subDescriptors.forall(d => d.matches(ref))
	}
	override def exampleMatch: Serializable = {
		subDescriptors.map(_.exampleMatch).find (e => this.matches(e)) match {
			case Some(e) => e
			case None => GameEntity.Sentinel
		}
	}
}
object CompoundDescriptor {
	def apply (desc : Traversable[TDescriptor]) = desc.size match {
		case 0 => TDescriptor.Sentinel
		case 1 => desc.head
		case more => new CompoundDescriptor(desc.toList)
	}
}