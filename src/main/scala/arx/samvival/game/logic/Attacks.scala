package arx.samvival.game.logic

import arx.core.math.Sext
import arx.engine.lworld._
import arx.samvival.game.actions.AttackAction
import arx.samvival.game.entities.Fields.{CharacterInfo, CombatData, Weapon}
import arx.samvival.game.entities.{AttackData, AttackProspect, AttackResult, CharacterInfo, CombatData, DamageResult, DamageType, Equipment, IdentityData, NoWeapon, Physical, StrikeOutcome, StrikeProspect, StrikeResult, Terrain, Tiles, Vegetation, Weapon}
import arx.samvival.game.events.GameEvents
import arx.core.introspection.FieldOperations._

object Attacks {

	val baseToHit = 8

	def attackProspect(action: AttackAction)(implicit world: LWorldView): List[AttackProspect] = {
		val attack = action.attack[AttackData]

		val weaponData = action.weapon.map(w => w[Weapon]).getOrElse(NoWeapon)
		val weaponDataBreakdown = action.weapon.map(w => world.dataModificationLog[Weapon](w)).getOrElse(new DataModificationLog[Weapon](NoWeapon, NoWeapon, Map()))
		val weaponName = action.weapon.map(w => Identity.name(w)).getOrElse("no weapon")

		val attacker = action.attacker
		val attackerChar = attacker[CharacterInfo]
		val attackerCombat = attacker[CombatData]
		val attackerTileEnt = Tiles.tileAt(attacker[Physical].position)
		val (attackerTerrain, attackerVegetation) = (attackerTileEnt[Terrain], attackerTileEnt[Vegetation])

		val attackerCombatBreakdown = world.dataModificationLog[CombatData](attacker)

		if (attack.strikeAPCost > attackerChar.actionPoints.currentValue) {
			return Nil
		}

		val validTargets = action.targets.filter(defender => {
			val dist = action.from.distance(defender[Physical].position)
			attack.minRange <= dist && dist <= attack.maxRange
		})

		val attackProspects = validTargets.map(defender => {

			val defenderCombat = defender[CombatData]
			val defenderTileEnt = Tiles.tileAt(defender[Physical].position)
			val (defenderTerrain, defenderVegetation) = (defenderTileEnt[Terrain], defenderTileEnt[Vegetation])

			val defenderCombatBreakdown = world.dataModificationLog[CombatData](defender)

			val strikeCount = attackerChar.actionPoints.currentValue / attack.strikeAPCost
			val strikeProspects = (0 until strikeCount.asInt).map(_ => {
				val baseStrike = baseStrikeProspect(action.attacker, action.attack, attackerCombatBreakdown, weaponDataBreakdown)

				var accuracy = baseStrike.accuracy
					.malus(defenderTerrain.cover, "defender terrain")

				if (attackerTerrain.elevation > defenderTerrain.elevation) {
					accuracy = accuracy.bonus(Sext(2), "high ground")
				} else if (attackerTerrain.elevation < defenderTerrain.elevation) {
					accuracy = accuracy.malus(Sext(2), "uphill attack")
				}

				val dodge = defenderCombatBreakdown.breakdownFor(CombatData.dodgeBonus, "base dodge bonus")

				val damageBonuses = baseStrike.damageBonuses

				val armor = defenderCombatBreakdown.breakdownFor(CombatData.armorBonus, "base armor bonus")

				val damageElementsBreakdown = baseStrike.damageElements

				StrikeProspect(accuracy, dodge, damageBonuses, armor, damageElementsBreakdown)
			})

			AttackProspect(attacker, defender, strikeProspects.toList)

		}).toList

		attackProspects
	}


	def baseStrikeProspect(attacker : LEntity, attack : LEntity, attackerCombatBreakdown : DataModificationLog[CombatData], weaponDataBreakdown : DataModificationLog[Weapon])(implicit view : LWorldView) : StrikeProspect = {
		val attackData = attack[AttackData]
		val weapon = attackData.weapon
		val weaponName = Identity.name(weapon)

		val accuracy = attackerCombatBreakdown.breakdownFor(CombatData.accuracyBonus, "base accuracy")
			.mergedWith(weaponDataBreakdown.breakdownFor(Weapon.accuracyBonus, "base weapon accuracy"), Some(s"[$weaponName]"), _ + _)

		val damage = attackerCombatBreakdown.breakdownFor(CombatData.damageBonuses, "base damage bonus")
			.mergedWith(weaponDataBreakdown.breakdownFor(Weapon.damageBonus, "base weapon damage bonus"), Some(s"[$weaponName]"), mergeDamageBonuses)

		val attackDamages = attackData.damage.values.toList
		val damageElementsBreakdown = Breakdown(attackDamages, "attack damage", attackDamages.map(d => d.toString).mkString(" + "), Impact.Positive)

		StrikeProspect(accuracy, Breakdown.empty(), damage, Breakdown.empty(), damageElementsBreakdown)
	}

	def mergeDamageBonuses (a : Map[DamageType, Sext], b : Map[DamageType, Sext]) : Map[DamageType, Sext] = {
		val allKeys = a.keys ++ b.keys
		allKeys.map(k => k -> (a.getOrElse(k, Sext(0)) + b.getOrElse(k, Sext(0)))).toMap
	}

	def attack(action: AttackAction)(implicit world: LWorld): List[AttackResult] = {
		implicit val view = world.view
		implicit val rand = Randomizer()

		val attack = action.attack[AttackData]

		attackProspect(action).map {
			prospect: AttackProspect =>
				val AttackProspect(attacker, defender, strikes) = prospect
				world.startEvent(GameEvents.Attack(prospect, None))

				val strikeResults = strikes.flatMap {
					strikeProspect: StrikeProspect =>
						if (!Combat.isDead(defender)) {
							val StrikeProspect(accuracy, dodge, damageBonuses, armor, damageElements) = strikeProspect
							if (Characters.spendAP(attacker, attack.strikeAPCost, "strike")) {
								world.startEvent(GameEvents.Strike(attacker, defender, strikeProspect, None))
								val toHit = rand.stdRoll().total + accuracy.total.asInt
								val result = if (toHit > baseToHit) {
									if (toHit > baseToHit + dodge.total.asInt) {
										val damageResults = damageElements.total.groupBy(de => de.damageType)
											.map {
												case (damageType, damageElementsOfType) => {
													val baseDamage = Sext(damageElementsOfType.map(de => de.damageDice.roll().total).sum)
													val modifiedDamage = baseDamage + damageBonuses.total.getOrElse(damageType, Sext(0))

													val damage = if (damageType.isA(DamageType.Physical)) {
														modifiedDamage - armor.total.asInt
													} else {
														modifiedDamage
													}

													DamageResult(damage.asInt, damageType)
												}
											}
											.toList

										val armorNullifiedDamage = damageResults.exists(dr => dr.amount <= 0 && dr.damageType.isA(DamageType.Physical))
										val outcomes: Set[StrikeOutcome] = if (armorNullifiedDamage) {
											Set(StrikeOutcome.Armored)
										} else {
											Set()
										}

										if (damageResults.nonEmpty && damageResults.map(dr => dr.amount).sum > 0) {
											Combat.applyDamage(defender, damageResults, "attack damage")

											StrikeResult(outcomes + StrikeOutcome.Hit, damageResults)
										} else {
											StrikeResult(outcomes, Nil)
										}
									} else {
										StrikeResult(Set(StrikeOutcome.Dodged), Nil)
									}
								} else {
									StrikeResult(Set(StrikeOutcome.Miss), Nil)
								}

								world.endEvent(GameEvents.Strike(attacker, defender, strikeProspect, Some(result)))
								Some(result)
							} else {
								None
							}
						} else {
							None
						}
				}

				val attackRes = AttackResult(attacker, defender, strikeResults)
				world.endEvent(GameEvents.Attack(prospect, Some(attackRes)))

				attackRes
		}
	}


	def availableAttacks(entity : LEntity)(implicit world : LWorldView) : Map[LEntity,List[LEntity]] = {
		val naturalWeapon = if (entity.hasData[Weapon]) { Set(entity) } else { Set() }
		val equippedWeapons = entity.dataOpt[Equipment].map(e => e.equipped.filter(item => item.hasData[Weapon])).getOrElse(Set())

		(equippedWeapons ++ naturalWeapon).map(w => w -> w[Weapon].attacks.values.toList).toMap
	}

	def canAttack(action : AttackAction)(implicit view : LWorldView) = {
		val prospects = Attacks.attackProspect(action)
		prospects.nonEmpty
	}
}
