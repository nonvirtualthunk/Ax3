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
import arx.engine.requirement.EntityDescriptor
import arx.engine.requirement.EntityRequirement
import arx.engine.requirement.Requirement

import scalaxy.loops._






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



