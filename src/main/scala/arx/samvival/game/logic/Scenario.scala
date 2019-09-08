package arx.samvival.game.logic

import arx.core.math.Sext
import arx.core.vec.coordinates.{AxialVec, AxialVec3}
import arx.engine.data.Reduceable
import arx.engine.lworld.{LEntity, LWorld}
import arx.samvival.game.entities.CharacterClass.Spearman
import arx.samvival.game.entities.Fields._
import arx.samvival.game.entities.Species.{Human, MudMonster}
import arx.samvival.game.entities.{AttackData, BodyPart, CharacterInfo, CombatData, DamageElement, DamageType, Equipment, FactionColors, IdentityData, Item, Levels, Physical, Taxonomy, Weapon}
import arx.samvival.game.events.GameEvents.EntityCreated
import arx.samvival.game.entities.DicePoolBuilder._
import arx.core.introspection.FieldOperations._
import arx.engine.event.GameEvent
import arx.samvival.game.entities.Taxonomy.AttackTypes

object Scenario {
	def loadTestScenario(implicit world: LWorld): Unit = {
		implicit val view = world.view
		val playerFaction = world.createEntity()
		world.attachData(playerFaction, Faction(f => {
			f.player = true
			f.color = FactionColors.Colors.head
		}))

		val enemyFaction = world.createEntity()
		world.attachData(enemyFaction, Faction(f => {
			f.player = false
			f.color = FactionColors.Colors.last
		}))

		world.attachWorldData(TurnState(t => {
			t.turnNumber = 1
			t.factionOrder = List(playerFaction, enemyFaction)
			t.activeFaction = playerFaction
		}))


		val mainCharacter = createCharacter(playerFaction, AxialVec3.Zero) {
			levels => levels.classLevels += Spearman -> 1
		} {
			char =>
				char.species = Human
				char.movePoints = Reduceable(Sext(10))
		}("Jimjam")

		val sword = createSword()

		Equipments.equip(mainCharacter, sword)
		val (weapon, attacks) = Attacks.availableAttacks(mainCharacter).head
		world.modify(mainCharacter, CombatData.activeAttack -> Some(attacks.head), None)
		world.addEvent(new GameEvent)


		val mudMonster = createCharacter(enemyFaction, AxialVec3(4, 0, 0)) {
			levels =>
		} {
			char =>
				char.species = MudMonster
				char.movePoints = Reduceable(Sext(4))
		}("mud monster")

		world.modify(mudMonster, CombatData.dodgeBonus + Sext(2), None)

	}


	def createSword()(implicit world: LWorld): LEntity = {
		val sword = world.createEntity()

		val slashAttack = world.createEntity()
		world.attachData(slashAttack).ofType[AttackData](a => {
			a.accuracyBonus = 1
			a.damage = Map("primary" -> DamageElement(1 d 6, DamageType.Slashing))
			a.strikeAPCost = Sext(3)
			a.weapon = sword
		}).ofType[IdentityData](i => {
			i.name = "slash"
			i.taxons = Set(AttackTypes.SlashAttack, AttackTypes.MeleeAttack)
		})

		world.attachData(sword).ofType[Weapon](w => {
			w.accuracyBonus = 1
			w.attacks = Map("primary" -> slashAttack)
			w.usesBodyParts = Map(BodyPart.Gripping -> 2, BodyPart.DextrousAppendage -> 2)
		}).ofType[IdentityData](i => {
			i.name = "test sword"
			i.taxons = Set(Taxonomy.Weapons.Sword)
		}).ofType[Item](i => {
			i.durability = Sext(30)
		})

		sword
	}

	def createCharacter(faction: LEntity, position: AxialVec3)(initLevels: Levels => Unit)(initCharacter: CharacterInfo => Unit)(name : String)(implicit world: LWorld): LEntity = {
		val newEnt = world.createEntity()
		world.attachData(newEnt, CharacterInfo(c => {
			c.faction = faction
			initCharacter(c)
		}))
		world.attachData(newEnt, Levels(initLevels))
		world.attachData(newEnt, new Physical)
		world.attachData(newEnt, new CombatData)
		world.attachData(newEnt, new Equipment)
		world.attachData(newEnt, IdentityData(i => i.name = Some(name)))
		world.attachData(newEnt, new Weapon)

 		world.addEvent(EntityCreated(newEnt))
		Movement.placeCharacterAt(newEnt, position)
		newEnt
	}
}
