package arx.eldr.game.requirements

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.engine.entity.TGameEntity
import scalaxy.loops._

object EntityDescriptor {
	def apply (f : (TGameEntity) => Boolean) = {
		new EntityDescriptor {
			override def matchesEntity(entity: TGameEntity): Boolean = f(entity)
		}
	}
}

trait EntityDescriptor extends EntityRequirement {
	amount = 1
	def matchesEntity (entity : TGameEntity) : Boolean
	override final def amountSatisfiedByEntity(entity: TGameEntity): Int = {
		if (matchesEntity(entity)) {
			1
		} else {
			0
		}
	}

	def and(other : EntityDescriptor) : EntityDescriptor = new CombinedEntityDescriptor(this, other)
}

class CombinedEntityDescriptor(a : EntityDescriptor, b : EntityDescriptor) extends EntityDescriptor {
	override def matchesEntity(entity: TGameEntity): Boolean = {
		a.matchesEntity(entity) && b.matchesEntity(entity)
	}
}