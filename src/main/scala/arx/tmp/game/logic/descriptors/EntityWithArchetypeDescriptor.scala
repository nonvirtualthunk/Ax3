package arx.tmp.game.logic.descriptors

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/21/13
 * Time: 3:27 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.core.representation.ConfigValue
import arx.tmp.game.logic.entities.core.GameArchetype
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.data.EntityFlag
import arx.tmp.game.logic.entities.data.FlagData

case class EntityWithArchetypeDescriptor(archetype : GameArchetype) extends TEntityDescriptor {
	override def name = archetype.name

	def matchesEntity(gameEntity: GameEntity): Boolean = {
		gameEntity.archetype == archetype
	}

	override def exampleMatch: GameEntity = GameEntity.Sentinel
}

class EntityWithFlagDescriptor(flags : Set[EntityFlag]) extends TEntityDescriptor {

	def matchesEntity(entity:GameEntity) : Boolean = {
		entity.auxDataOpt[FlagData] match {
			case Some(fd) => flags.forall( fd.flags.contains )
			case None => false
		}
	}

	override def exampleMatch : GameEntity = GameEntity.Sentinel

}

class EntityWithoutFlagDescriptor(flags : Set[EntityFlag]) extends TEntityDescriptor {

	def matchesEntity(entity:GameEntity) : Boolean = {
		val hasFlags = (entity.auxDataOpt[FlagData] match {
			case Some(fd) => flags.forall( fd.flags.contains )
			case None => false
		})
		! hasFlags
	}

	override def exampleMatch : GameEntity = GameEntity.Sentinel

}

object EntityWithFlagDescriptor extends TConfiguredDescriptorParser {
	def apply (flags : Set[EntityFlag]) = new EntityWithFlagDescriptor(flags)


	override def parseFromSML(sml: ConfigValue): Option[TDescriptor] = {
		if (sml.flags.nonEmptyValue) {
			val flags = sml.flags.arr.map(_.str).toSet.map(EntityFlag.fromString)
			Some(new EntityWithFlagDescriptor(flags))
		} else {
			None
		}
	}
	override def kindStrings: List[String] = List("ItemWithFlag","EntityWithFlag")
	override def parseFromString(arg: String): TDescriptor = new EntityWithFlagDescriptor(arg.split(",").toSet.map(EntityFlag.fromString))
}

case class SpecificArchetypeDescriptor(archetype : GameArchetype) extends TArchetypeDescriptor {
	override def matchesArchetype(gameArchetype: GameArchetype): Boolean = gameArchetype == archetype
	override def exampleMatch: GameArchetype = archetype
}