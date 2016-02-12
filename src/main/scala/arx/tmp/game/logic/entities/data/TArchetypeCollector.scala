package arx.tmp.game.logic.entities.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/14/14
 * Time: 8:50 AM
 */

import arx.application.Noto
import arx.core.representation.ConfigValue
import arx.tmp.game.logic.entities.core.ConfigurableEntityAssistant
import arx.tmp.game.logic.entities.core.ConfigurableGameArchetype
import arx.tmp.game.logic.entities.core.GameArchetype

trait TArchetypeCollector[T <: GameArchetype] {
	protected def baseSMLLocation : String
	protected def typeKey : String
	protected def clazz : Class[T]
	protected def fromSML[U >: T] ( name : String, sml : ConfigValue ) : U

	lazy val allSML = ConfigurableEntityAssistant.loadAllConfigsByPackage(baseSMLLocation,typeKey)
	protected lazy val _discoveredArchetypes = loadSMLArchetypes() ::: ReflectionAssistant.instancesOfSubtypesOf(clazz)
	protected var registeredArchetypes = List[T]()
	protected lazy val _archetypesByName = ConfigurableGameArchetype.organizeByName(_discoveredArchetypes)
	def allArchetypes = _discoveredArchetypes ::: registeredArchetypes
	def withName (name : String) = archetypeWithName(name)
	def archetypeWithName( name : String ) = _archetypesByName( name.toLowerCase )
	def archetypeWithNameExists( name : String ) = _archetypesByName.contains( name.toLowerCase )
	def getOrElseCreate (name : String,arch : => T) = {
		if (! _archetypesByName.contains(name)) {
			val newArch : T = arch
			if (! newArch.name.equalsIgnoreCase(name)) { Noto.error(s"getOrElseCreate archetype called, but created arch had wrong name, ${newArch.name} not $name") }
			else {
				_archetypesByName.put(name,newArch)
				registeredArchetypes ::= newArch
			}
			newArch
		} else {
			_archetypesByName(name)
		}
	}
	def registerArchetype(name : String,arch : => T) = getOrElseCreate(name,arch)

	protected def loadSMLArchetypes () = {
		val smlArchs = allSML.map { case (name,sml) => val b = fromSML(name.toString,sml); b.name = name.toString; b }

		smlArchs.toList
	}

	def couldHoldInstancesOfClass(c : Class[_]) = clazz.isAssignableFrom(c)
}
