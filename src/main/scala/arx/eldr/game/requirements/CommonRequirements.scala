package arx.eldr.game.requirements

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.eldr.game.entity.data.ItemData
import arx.eldr.game.entity.data.ItemFlag
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.GameArchetype
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity
import scalaxy.loops._

class CommonRequirements {

}

object AuxDataDescriptor {
	def apply[T <: TGameEntityAuxData : Manifest] (f : (T) => Boolean) : EntityDescriptor = new EntityDescriptor {
		override def matchesEntity(entity: TGameEntity): Boolean = entity.hasAuxData[T] && f(entity[T])
	}
	def apply[T <: TGameEntityAuxData : Manifest] : EntityDescriptor = apply[T]((x:T) => true)
}

object AnyOneRequirement extends Requirement {
	amount = 1
	override def amountSatisfiedBy(entity: Any): Int = 1
}

object SpecificEntityDescriptor {
	def apply (ent : TGameEntity) = new SpecificEntityDescriptor(ent)
}
class SpecificEntityDescriptor(ent:TGameEntity) extends EntityDescriptor{
	override def matchesEntity(entity: TGameEntity): Boolean = entity == ent
}

case class ItemWithFlagRequirement(flag : ItemFlag, amnt : Int = 1) extends EntityRequirement {
	amount = amnt
	override def amountSatisfiedByEntity(entity: TGameEntity): Int = entity.auxDataOpt[ItemData] match {
		case Some(itemData) if itemData.flags.contains(flag) => 1
		case _ => 0
	}
}

case class ItemWithFlagDescriptor(flag : ItemFlag) extends EntityDescriptor {
	override def matchesEntity(entity: TGameEntity): Boolean = entity.auxDataOpt[ItemData] match {
		case Some(itemData) if itemData.flags.contains(flag) => true
		case _ => false
	}
}

case class EntityWithArchetypeDescriptor (archetype : GameArchetype) extends EntityDescriptor {
	override def matchesEntity(entity: TGameEntity): Boolean = entity.archetype.contains(archetype)
}

/**
  * Requirement that selects for only an individual entity, as opposed to an archetype
  */
object IndividualEntityDescriptor extends EntityDescriptor {
	override def matchesEntity(entity: TGameEntity): Boolean = {
		entity match {
			case ga : GameArchetype => false
			case _ => true
		}
	}
}