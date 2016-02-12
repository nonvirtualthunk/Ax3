package arx.axistential.game.archetypes.item

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/11/15
 * Time: 8:51 AM
 */

import arx.axistential.game.archetypes.traits.TPhysicalEntityArchetype
import arx.core.representation.ConfigValue
import arx.tmp.game.logic.entities.core.GameArchetype
import arx.tmp.game.logic.entities.data.TArchetypeCollector


abstract class ItemArchetype extends GameArchetype with TPhysicalEntityArchetype {
	/**
	 * Determines whether this kind of item can be built directly in the world, as opposed to
	 * being crafted, then either carried or placed somewhere. Workshop items (crafting table,
	 * anvil, etc) and large furniture are
	 * free standing. Most other things (books, glasses, swords, etc) are not.
	 * <br />
	 * In general, this will only affect UI, particularly how a command to create an item will
	 * be given and location chosen.
	 * <br />
	 * It is assumed that free standing items will not have any recipes that require a location
	 * to be built at (no crafting recipe for an anvil that requires it to be made at a
	 * crafting table, for example), though requiring a location <i>near</i> something else is
	 * acceptable.
	 */
	var freeStanding = false

	/**
	 * Starting point for economic value of this kind of item, expressed unitless for the moment
	 */
	var baseValue = Moddable(0)

	/**
	 * Collection of categories this item archetype can be said to belong to, used for broad
	 * organizational purposes, not in-game considerations. For that use EntityFlag's.
	 */
	var itemCategories = Set[String]()

	/**
	 * Organizational designation more specific than category. Used when several archetypes are
	 * close variations on a central concept. For example both "Straw Rope" and "Silk Rope" would be
	 * subtypeOf "Rope". Not for in-game use, intended for organizational purposes only.
	 */
	var subtypeOf : Option[String] = None
}

//object ItemArchetype {
//	lazy val allSML = SMLEntityAssistant.loadAllConfigsByPackage("axis/entities/items/Items.sml","items")
//	protected lazy val _discoveredArchetypes = loadSMLArchetypes() ::: ReflectionAssistant.instancesOfSubtypesOf[ItemArchetype]
//	protected var registeredArchetypes = List[ItemArchetype]()
//	def allArchetypes = _discoveredArchetypes ::: registeredArchetypes
//	protected lazy val _archetypesByName = SMLGameArchetype.organizeByName(allArchetypes)
//	def withName (name : String) = archetypeWithName(name)
//	def archetypeWithName( name : String ) = _archetypesByName( name )
//	def archetypeWithNameExists( name : String ) = _archetypesByName.contains( name )
//	def registerArchetype( arch : ItemArchetype ) : ItemArchetype = {
//		if ( ! _archetypesByName.contains(arch.name) ) {
//			_archetypesByName(arch.name) = arch
//			arch
//		} else {
//			Noto.info(s"Registering archetype ${arch.name}, but already exists, returning old")
//			_archetypesByName(arch.name)
//		}
//	}
//	def getOrCreate ( name : String , arch : => ItemArchetype ) = {
//		_archetypesByName.getOrElseUpdate(name,arch)
//	}
//
//	def loadSMLArchetypes () = {
//		val smlArchs = allSML.map { case (name,sml) => {
//			val arch = new SMLItemArchetype(sml)
//			arch.name = name.toString
//			arch
//		}}
//
//		smlArchs.toList
//	}
//}

object ItemArchetype extends TArchetypeCollector[ItemArchetype] {
	override protected def baseSMLLocation: String = "axis/entities/items/Items.sml"
	override protected def typeKey: String = "items"
	override protected def fromSML[U >: ItemArchetype](name: String, sml: ConfigValue): U = {
		val arch = new ConfiguredItemArchetype(sml)
		arch.name = name
		arch
	}
	override protected def clazz: Class[ItemArchetype] = classOf[ItemArchetype]
}