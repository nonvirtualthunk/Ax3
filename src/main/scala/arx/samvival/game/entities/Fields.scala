package arx.samvival.game.entities

import arx.core.introspection.{Clazz, Field}
import arx.engine.lworld.{LEntity, LWorld}

object Fields {

	object TurnState extends Clazz[TurnState]("TurnState", classOf[TurnState]){
		val Sentinel = new TurnState
		val turnNumber = Field.fromValue(Sentinel.turnNumber).createField[TurnState]("turnNumber",f => f.turnNumber, (f,turnNumber) => f.turnNumber = turnNumber, TurnState)
		fields += "turnNumber" -> turnNumber
		val activeFaction = Field.fromValue(Sentinel.activeFaction).createField[TurnState]("activeFaction",f => f.activeFaction, (f,activeFaction) => f.activeFaction = activeFaction, TurnState)
		fields += "activeFaction" -> activeFaction
		val factionOrder = Field.fromValue(Sentinel.factionOrder).createField[TurnState]("factionOrder",f => f.factionOrder, (f,factionOrder) => f.factionOrder = factionOrder, TurnState)
		fields += "factionOrder" -> factionOrder

		def apply(f : TurnState => Unit) : TurnState = { val v = new TurnState; f(v); v }

	}
	object Vegetation extends Clazz[Vegetation]("Vegetation", classOf[Vegetation]){
		val Sentinel = new Vegetation
		val cover = Field.fromValue(Sentinel.cover).createField[Vegetation]("cover",f => f.cover, (f,cover) => f.cover = cover, Vegetation)
		fields += "cover" -> cover
		val moveCost = Field.fromValue(Sentinel.moveCost).createField[Vegetation]("moveCost",f => f.moveCost, (f,moveCost) => f.moveCost = moveCost, Vegetation)
		fields += "moveCost" -> moveCost
		val kind = Field.fromValue(Sentinel.kind).createField[Vegetation]("kind",f => f.kind, (f,kind) => f.kind = kind, Vegetation)
		fields += "kind" -> kind

		def apply(f : Vegetation => Unit) : Vegetation = { val v = new Vegetation; f(v); v }

	}
	object Item extends Clazz[Item]("Item", classOf[Item]){
		val Sentinel = new Item
		val wornOn = Field.fromValue(Sentinel.wornOn).createField[Item]("wornOn",f => f.wornOn, (f,wornOn) => f.wornOn = wornOn, Item)
		fields += "wornOn" -> wornOn
		val durability = Field.fromValue(Sentinel.durability).createField[Item]("durability",f => f.durability, (f,durability) => f.durability = durability, Item)
		fields += "durability" -> durability

		def apply(f : Item => Unit) : Item = { val v = new Item; f(v); v }

	}
	object Faction extends Clazz[Faction]("Faction", classOf[Faction]){
		val Sentinel = new Faction
		val color = Field.fromValue(Sentinel.color).createField[Faction]("color",f => f.color, (f,color) => f.color = color, Faction)
		fields += "color" -> color
		val player = Field.fromValue(Sentinel.player).createField[Faction]("player",f => f.player, (f,player) => f.player = player, Faction)
		fields += "player" -> player

		def apply(f : Faction => Unit) : Faction = { val v = new Faction; f(v); v }

	}
	object AttackData extends Clazz[AttackData]("Attack", classOf[AttackData]){
		val Sentinel = new AttackData
		val weapon = Field.fromValue(Sentinel.weapon).createField[AttackData]("weapon", f => f.weapon, (f, weapon) => f.weapon = weapon, AttackData)
		fields += "weapon" -> weapon
		val accuracyBonus = Field.fromValue(Sentinel.accuracyBonus).createField[AttackData]("accuracyBonus", f => f.accuracyBonus, (f, accuracyBonus) => f.accuracyBonus = accuracyBonus, AttackData)
		fields += "accuracyBonus" -> accuracyBonus
		val strikeAPCost = Field.fromValue(Sentinel.strikeAPCost).createField[AttackData]("strikeAPCost", f => f.strikeAPCost, (f, strikeAPCost) => f.strikeAPCost = strikeAPCost, AttackData)
		fields += "strikeAPCost" -> strikeAPCost
		val staminaCost = Field.fromValue(Sentinel.staminaCost).createField[AttackData]("staminaCost", f => f.staminaCost, (f, staminaCost) => f.staminaCost = staminaCost, AttackData)
		fields += "staminaCost" -> staminaCost
		val minRange = Field.fromValue(Sentinel.minRange).createField[AttackData]("minRange", f => f.minRange, (f, minRange) => f.minRange = minRange, AttackData)
		fields += "minRange" -> minRange
		val maxRange = Field.fromValue(Sentinel.maxRange).createField[AttackData]("maxRange", f => f.maxRange, (f, maxRange) => f.maxRange = maxRange, AttackData)
		fields += "maxRange" -> maxRange
		val damage = Field.fromValue(Sentinel.damage).createField[AttackData]("damage", f => f.damage, (f, damage) => f.damage = damage, AttackData)
		fields += "damage" -> damage

		def apply(f : AttackData => Unit) : AttackData = { val v = new AttackData; f(v); v }

	}
	object Levels extends Clazz[Levels]("Levels", classOf[Levels]){
		val Sentinel = new Levels
		val skillBonuses = Field.fromValue(Sentinel.skillBonuses).createField[Levels]("skillBonuses",f => f.skillBonuses, (f,skillBonuses) => f.skillBonuses = skillBonuses, Levels)
		fields += "skillBonuses" -> skillBonuses
		val skillXp = Field.fromValue(Sentinel.skillXp).createField[Levels]("skillXp",f => f.skillXp, (f,skillXp) => f.skillXp = skillXp, Levels)
		fields += "skillXp" -> skillXp
		val classLevels = Field.fromValue(Sentinel.classLevels).createField[Levels]("classLevels",f => f.classLevels, (f,classLevels) => f.classLevels = classLevels, Levels)
		fields += "classLevels" -> classLevels

		def apply(f : Levels => Unit) : Levels = { val v = new Levels; f(v); v }

	}
	object Equipment extends Clazz[Equipment]("Equipment", classOf[Equipment]){
		val Sentinel = new Equipment
		val equipped = Field.fromValue(Sentinel.equipped).createField[Equipment]("equipped",f => f.equipped, (f,equipped) => f.equipped = equipped, Equipment)
		fields += "equipped" -> equipped

		def apply(f : Equipment => Unit) : Equipment = { val v = new Equipment; f(v); v }

	}
	object Physical extends Clazz[Physical]("Physical", classOf[Physical]){
		val Sentinel = new Physical
		val position = Field.fromValue(Sentinel.position).createField[Physical]("position",f => f.position, (f,position) => f.position = position, Physical)
		fields += "position" -> position
		val exactPositionOverride = Field.fromValue(Sentinel.exactPositionOverride).createField[Physical]("exactPositionOverride",f => f.exactPositionOverride, (f,exactPositionOverride) => f.exactPositionOverride = exactPositionOverride, Physical)
		fields += "exactPositionOverride" -> exactPositionOverride
		val colorMultiplier = Field.fromValue(Sentinel.colorMultiplier).createField[Physical]("colorMultiplier",f => f.colorMultiplier, (f,colorMultiplier) => f.colorMultiplier = colorMultiplier, Physical)
		fields += "colorMultiplier" -> colorMultiplier
		val facing = Field.fromValue(Sentinel.facing).createField[Physical]("facing",f => f.facing, (f,facing) => f.facing = facing, Physical)
		fields += "facing" -> facing
		val occupiesHex = Field.fromValue(Sentinel.occupiesHex).createField[Physical]("occupiesHex",f => f.occupiesHex, (f,occupiesHex) => f.occupiesHex = occupiesHex, Physical)
		fields += "occupiesHex" -> occupiesHex

		def apply(f : Physical => Unit) : Physical = { val v = new Physical; f(v); v }

	}
	object CharacterInfo extends Clazz[CharacterInfo]("CharacterInfo", classOf[CharacterInfo]){
		val Sentinel = new CharacterInfo
		val species = Field.fromValue(Sentinel.species).createField[CharacterInfo]("species",f => f.species, (f,species) => f.species = species, CharacterInfo)
		fields += "species" -> species
		val faction = Field.fromValue(Sentinel.faction).createField[CharacterInfo]("faction",f => f.faction, (f,faction) => f.faction = faction, CharacterInfo)
		fields += "faction" -> faction
		val health = Field.fromValue(Sentinel.health).createField[CharacterInfo]("health",f => f.health, (f,health) => f.health = health, CharacterInfo)
		fields += "health" -> health
		val healthRecoveryRate = Field.fromValue(Sentinel.healthRecoveryRate).createField[CharacterInfo]("healthRecoveryRate",f => f.healthRecoveryRate, (f,healthRecoveryRate) => f.healthRecoveryRate = healthRecoveryRate, CharacterInfo)
		fields += "healthRecoveryRate" -> healthRecoveryRate
		val alive = Field.fromValue(Sentinel.alive).createField[CharacterInfo]("alive",f => f.alive, (f,alive) => f.alive = alive, CharacterInfo)
		fields += "alive" -> alive
		val actionPoints = Field.fromValue(Sentinel.actionPoints).createField[CharacterInfo]("actionPoints",f => f.actionPoints, (f,actionPoints) => f.actionPoints = actionPoints, CharacterInfo)
		fields += "actionPoints" -> actionPoints
		val moveSpeed = Field.fromValue(Sentinel.moveSpeed).createField[CharacterInfo]("moveSpeed",f => f.moveSpeed, (f,moveSpeed) => f.moveSpeed = moveSpeed, CharacterInfo)
		fields += "moveSpeed" -> moveSpeed
		val movePoints = Field.fromValue(Sentinel.movePoints).createField[CharacterInfo]("movePoints",f => f.movePoints, (f,movePoints) => f.movePoints = movePoints, CharacterInfo)
		fields += "movePoints" -> movePoints
		val bodyParts = Field.fromValue(Sentinel.bodyParts).createField[CharacterInfo]("bodyParts",f => f.bodyParts, (f,bodyParts) => f.bodyParts = bodyParts, CharacterInfo)
		fields += "bodyParts" -> bodyParts
		val strength = Field.fromValue(Sentinel.strength).createField[CharacterInfo]("strength",f => f.strength, (f,strength) => f.strength = strength, CharacterInfo)
		fields += "strength" -> strength
		val dexterity = Field.fromValue(Sentinel.dexterity).createField[CharacterInfo]("dexterity",f => f.dexterity, (f,dexterity) => f.dexterity = dexterity, CharacterInfo)
		fields += "dexterity" -> dexterity
		val intellect = Field.fromValue(Sentinel.intellect).createField[CharacterInfo]("intellect",f => f.intellect, (f,intellect) => f.intellect = intellect, CharacterInfo)
		fields += "intellect" -> intellect
		val cunning = Field.fromValue(Sentinel.cunning).createField[CharacterInfo]("cunning",f => f.cunning, (f,cunning) => f.cunning = cunning, CharacterInfo)
		fields += "cunning" -> cunning

		def apply(f : CharacterInfo => Unit) : CharacterInfo = { val v = new CharacterInfo; f(v); v }

	}
	object IdentityData extends Clazz[IdentityData]("IdentityData", classOf[IdentityData]){
		val Sentinel = new IdentityData
		val name = Field.fromValue(Sentinel.name).createField[IdentityData]("name",f => f.name, (f,name) => f.name = name, IdentityData)
		fields += "name" -> name
		val taxons = Field.fromValue(Sentinel.taxons).createField[IdentityData]("taxons",f => f.taxons, (f,taxons) => f.taxons = taxons, IdentityData)
		fields += "taxons" -> taxons

		def apply(f : IdentityData => Unit) : IdentityData = { val v = new IdentityData; f(v); v }

	}
	object Tile extends Clazz[Tile]("Tile", classOf[Tile]){
		val Sentinel = new Tile
		val entities = Field.fromValue(Sentinel.entities).createField[Tile]("entities",f => f.entities, (f,entities) => f.entities = entities, Tile)
		fields += "entities" -> entities
		val position = Field.fromValue(Sentinel.position).createField[Tile]("position",f => f.position, (f,position) => f.position = position, Tile)
		fields += "position" -> position

		def apply(f : Tile => Unit) : Tile = { val v = new Tile; f(v); v }

	}
	object CombatData extends Clazz[CombatData]("CombatData", classOf[CombatData]){
		val Sentinel = new CombatData
		val accuracyBonus = Field.fromValue(Sentinel.accuracyBonus).createField[CombatData]("accuracyBonus",f => f.accuracyBonus, (f,accuracyBonus) => f.accuracyBonus = accuracyBonus, CombatData)
		fields += "accuracyBonus" -> accuracyBonus
		val damageBonuses = Field.fromValue(Sentinel.damageBonuses).createField[CombatData]("damageBonuses",f => f.damageBonuses, (f,damageBonuses) => f.damageBonuses = damageBonuses, CombatData)
		fields += "damageBonuses" -> damageBonuses
		val dodgeBonus = Field.fromValue(Sentinel.dodgeBonus).createField[CombatData]("dodgeBonus",f => f.dodgeBonus, (f,dodgeBonus) => f.dodgeBonus = dodgeBonus, CombatData)
		fields += "dodgeBonus" -> dodgeBonus
		val armorBonus = Field.fromValue(Sentinel.armorBonus).createField[CombatData]("armorBonus",f => f.armorBonus, (f,armorBonus) => f.armorBonus = armorBonus, CombatData)
		fields += "armorBonus" -> armorBonus
		val activeAttack = Field.fromValue(Sentinel.activeAttack).createField[CombatData]("activeAttack",f => f.activeAttack, (f,activeAttack) => f.activeAttack = activeAttack, CombatData)
		fields += "activeAttack" -> activeAttack

		def apply(f : CombatData => Unit) : CombatData = { val v = new CombatData; f(v); v }

	}
	object Terrain extends Clazz[Terrain]("Terrain", classOf[Terrain]){
		val Sentinel = new Terrain
		val fertility = Field.fromValue(Sentinel.fertility).createField[Terrain]("fertility",f => f.fertility, (f,fertility) => f.fertility = fertility, Terrain)
		fields += "fertility" -> fertility
		val cover = Field.fromValue(Sentinel.cover).createField[Terrain]("cover",f => f.cover, (f,cover) => f.cover = cover, Terrain)
		fields += "cover" -> cover
		val elevation = Field.fromValue(Sentinel.elevation).createField[Terrain]("elevation",f => f.elevation, (f,elevation) => f.elevation = elevation, Terrain)
		fields += "elevation" -> elevation
		val moveCost = Field.fromValue(Sentinel.moveCost).createField[Terrain]("moveCost",f => f.moveCost, (f,moveCost) => f.moveCost = moveCost, Terrain)
		fields += "moveCost" -> moveCost
		val kind = Field.fromValue(Sentinel.kind).createField[Terrain]("kind",f => f.kind, (f,kind) => f.kind = kind, Terrain)
		fields += "kind" -> kind

		def apply(f : Terrain => Unit) : Terrain = { val v = new Terrain; f(v); v }

	}
	object Weapon extends Clazz[Weapon]("Weapon", classOf[Weapon]){
		val Sentinel = new Weapon
		val attacks = Field.fromValue(Sentinel.attacks).createField[Weapon]("attacks",f => f.attacks, (f,attacks) => f.attacks = attacks, Weapon)
		fields += "attacks" -> attacks
		val accuracyBonus = Field.fromValue(Sentinel.accuracyBonus).createField[Weapon]("accuracyBonus",f => f.accuracyBonus, (f,accuracyBonus) => f.accuracyBonus = accuracyBonus, Weapon)
		fields += "accuracyBonus" -> accuracyBonus
		val damageBonus = Field.fromValue(Sentinel.damageBonus).createField[Weapon]("damageBonus",f => f.damageBonus, (f,damageBonus) => f.damageBonus = damageBonus, Weapon)
		fields += "damageBonus" -> damageBonus
		val extraDamage = Field.fromValue(Sentinel.extraDamage).createField[Weapon]("extraDamage",f => f.extraDamage, (f,extraDamage) => f.extraDamage = extraDamage, Weapon)
		fields += "extraDamage" -> extraDamage
		val usesBodyParts = Field.fromValue(Sentinel.usesBodyParts).createField[Weapon]("usesBodyParts",f => f.usesBodyParts, (f,usesBodyParts) => f.usesBodyParts = usesBodyParts, Weapon)
		fields += "usesBodyParts" -> usesBodyParts

		def apply(f : Weapon => Unit) : Weapon = { val v = new Weapon; f(v); v }

	}
	object Inventory extends Clazz[Inventory]("Inventory", classOf[Inventory]){
		val Sentinel = new Inventory
		val heldItems = Field.fromValue(Sentinel.heldItems).createField[Inventory]("heldItems",f => f.heldItems, (f,heldItems) => f.heldItems = heldItems, Inventory)
		fields += "heldItems" -> heldItems
		val itemCountLimit = Field.fromValue(Sentinel.itemCountLimit).createField[Inventory]("itemCountLimit",f => f.itemCountLimit, (f,itemCountLimit) => f.itemCountLimit = itemCountLimit, Inventory)
		fields += "itemCountLimit" -> itemCountLimit

		def apply(f : Inventory => Unit) : Inventory = { val v = new Inventory; f(v); v }

	}

	def registerTypes(world: LWorld) : Unit = {
		world.register[TurnState]
		world.register[Vegetation]
		world.register[Item]
		world.register[Faction]
		world.register[AttackData]
		world.register[Levels]
		world.register[Equipment]
		world.register[Physical]
		world.register[CharacterInfo]
		world.register[IdentityData]
		world.register[Tile]
		world.register[CombatData]
		world.register[Terrain]
		world.register[Weapon]
		world.register[Inventory]
	}
}