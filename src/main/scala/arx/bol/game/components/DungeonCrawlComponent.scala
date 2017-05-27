package arx.bol.game.components

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.bol.game.components.EntityTypes.Creature
import arx.bol.game.entities.ItemArchetype
import arx.bol.game.entities.PlayerCharacter
import arx.bol.game.entities.WeaponArchetype
import arx.bol.game.entities.Weapons
import arx.bol.game.entities.data.ActionType
import arx.bol.game.entities.data.Behavior
import arx.bol.game.entities.data.CreatureData
import arx.bol.game.entities.data.PhysicalData
import arx.bol.game.world.data.DungeonData
import arx.core.units.UnitOfTime
import arx.core.vec.Vec2f
import arx.core.vec.Vec2i
import arx.engine.entity.GameArchetype
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent

import scalaxy.loops._

object EntityTypes {
	implicit class Creature(val gameEntity : TGameEntity) {
		final val cd = gameEntity[CreatureData]
		final val pd = gameEntity[PhysicalData]
	}
	implicit def creatureToCD(c : Creature) : CreatureData = c.cd
	implicit def creatureToPD(c : Creature) : PhysicalData = c.pd
	implicit def creatureToEntity(c : Creature) : TGameEntity = c.gameEntity
}


class DungeonCrawlComponent(engine : GameEngine) extends GameComponent(engine) {
	val creatureQuery = world.auxDataQuery[CreatureData]
	def pcQuery = creatureQuery.filter(c => c[CreatureData].behavior == Behavior.PlayerCharacter)

	override protected def update(dt: UnitOfTime): Unit = {
		val dungeon = world[DungeonData].activeDungeon


		pcQuery.headOption match {
			case Some(pcRaw) =>
				val pc : Creature = pcRaw
				val pcLoc = pc.location
				val pcSize = pc.size


				// update all creatures
				for (creatureRaw <- creatureQuery) {
					val creature : Creature = creatureRaw
					creature.behavior match {
						case Behavior.PlayerCharacter =>
							val target = creatureQuery.toStream
								   .filter(e => ! (e eq pcRaw))
								   .filter(e => e[PhysicalData].location.x > pcLoc.x)
								   .sortBy(e => e[PhysicalData].location.x)
							   	.headOption
							   	.filter(e => e[PhysicalData].location.x < pcLoc.x + pcSize.x + 0.1f)

							target match {
								case Some(enemyRaw) =>
									val enemy : Creature = enemyRaw
									creature.currentAction = ActionType.Attack
									performAttack(creature, enemy, dt)
								case None =>
									creature.currentAction = ActionType.Walk
									creature.actionProgress += dt.inSeconds
									if (creature.actionProgress >= 1.0f) {
										creature.actionProgress -= 1.0f
									}
									creature.location = creature.location + Vec2f((creature.speed * dt).inVoxels,0.0f)
							}
						case Behavior.AttackWhenNear =>
							val loc = creature.location
							val lDist = (loc.x - pcLoc.x).abs
							val rDist = (loc.x - (pcLoc.x + pcSize.x)).abs
							if (lDist.min(rDist) < 0.1) {
								performAttack(creature,pc, dt)
							} else {
								creature.currentAction = ActionType.Wait
							}

					}

					var maxY = -10000
					for (i <- creature.location.x.floor.toInt to (creature.location.x + creature.size.x).ceil.toInt) {
						maxY = maxY.max(dungeon.heightmap(i))
					}
					creature.location = Vec2f(creature.location.x,maxY )
				}
			case None => Noto.error("No pc in world")
		}
	}

	def performAttack(attacker : Creature, defender : Creature, dt : UnitOfTime): Unit = {
		attacker.currentAction = ActionType.Attack
		attacker.actionProgress += (dt.inSeconds / attacker.attackTime.inSeconds)
		if (attacker.actionProgress >= 1.0f) {
			defender.hp = defender.hp - attacker.attackDamage
			if (defender.hp <= 0) {
				world.removeEntity(defender)

				val allItems = GameArchetype.archetypes(ItemArchetype).values.toSeq
				val loot = randFrom(allItems).create()
				loot[PhysicalData].location = defender.location
				world.addEntities(loot)
			}

			attacker.actionProgress = 0.0f
		}
	}
}
