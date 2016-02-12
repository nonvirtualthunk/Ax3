package arx.axistential.game.data.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/11/13
 * Time: 2:37 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.data.entity.AIData.EnemyMemory
import arx.axistential.game.entities.CreatureEntity
import arx.axistential.game.logic.general.FriendOrFoeLogic._
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.ObjectCoord
import arx.tmp.game.logic.entities.core.TGameBase
import arx.tmp.game.logic.entities.data.TGameEntityAuxData

class AIData extends TGameEntityAuxData {
	protected var forEntity = CreatureEntity.Sentinel
//	var enemiesInSight : Set[CreatureEntity] = Set()
	var detectedCreatures : Set[CreatureEntity] = Set()
	def detectedEnemies = detectedCreatures.filter(_.isEnemyOf(forEntity))

	protected var _rememberedEnemies = List[EnemyMemory]()
	/** The amount of time to keep memories of non-observable enemies */
	var memoryLimit = 0.1.cycle
	def rememberedEnemies = {
		_rememberedEnemies = _rememberedEnemies.filter(m => forEntity.worldTime - m.timestamp < memoryLimit)
		_rememberedEnemies
	}
	def addEnemyMemory (mem : EnemyMemory): Unit = {
		_rememberedEnemies ::= mem
	}
	def removeEnemyMemory (creature : CreatureEntity): Unit = {
		_rememberedEnemies = _rememberedEnemies.filterNot(_.creature == creature)
	}


	override def onAssignedToEntity(entity: TGameBase): Unit = {
		entity match {
			case cr : CreatureEntity => forEntity = cr
			case o => Noto.warn(s"Invalid type of entity making use of AIData: $o")
		}
	}
}

object AIData {
	/** Record of an enemy that has been seen but is no longer in observation range */
	case class EnemyMemory (creature : CreatureEntity, lastObserved : ObjectCoord, timestamp : UnitOfTime)
}