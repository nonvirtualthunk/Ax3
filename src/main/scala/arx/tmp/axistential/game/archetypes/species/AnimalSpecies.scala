package arx.axistential.game.archetypes.species

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/5/13
 * Time: 10:23 AM
 */

import java.io.ObjectInputStream
import java.io.ObjectOutputStream

import arx.axistential.game.data.entity._
import arx.axistential.game.data.entity.animal.AnimalData
import arx.axistential.game.data.world.Season
import arx.axistential.game.entities.CreatureEntity
import arx.axistential.game.entities.traits.SimplePhysicalEntity
import arx.core.representation.ConfigValue
import arx.core.traits.TSentinel
import arx.tmp.game.logic.entities.data._
import arx.requirements.NoRequirement

class AnimalSpecies extends Species with TConfiguredSpecies {
	def this(conf : ConfigValue) { this () ; setFromSML(conf) }

	var animalKind : AnimalKind = AnimalKind("animal")

	var matingSeasons : Set[Season] = Season.AllYear
	var burrowType : BurrowType = BurrowType.None
	var animalGroupType : AnimalGroupType = AnimalGroupType.Solitary
	var sentient = false

	override def speciesKind: SpeciesKind = animalKind
	override def createInstance : CreatureEntity = super.createInstance.asInstanceOf[CreatureEntity]

	override def setFromSML(conf: ConfigValue, overwrite: Boolean = true): Unit = {
		_auxData.clear()
		applyConfigurableAuxData(conf,this)

		name = conf.name.strOrElse(name)

		animalKind = AnimalKind(conf.kind.strOrElse("animal"))

		sentient = conf.sentient.boolOrElse(sentient)
		burrowType = conf.burrowType match {
			case ConfigValue.Sentinel => burrowType
			case bt => BurrowType(bt.str)
		}

		val seasonsRaw = extractSingularOrPlural(conf,"matingSeason","matingSeasons")
		matingSeasons =	if ( seasonsRaw.isEmpty ) { matingSeasons }
		else { seasonsRaw.map( raw => Season.fromString( raw.str ) ).toSet }

		animalGroupType = conf.animalGroupType match {
			case ConfigValue.Sentinel => animalGroupType
			case agt => AnimalGroupType(agt.str)
		}

		parseAgeCategoryStartTimes(conf)
		parseAgeCategoryCollisionShapes(conf)
		parseAgeCategoryInformation(conf)
	}

	override protected def createPhysicalInstance = {
		val creature = new CreatureEntity
		creature.species = this
		val AD = creature.aux[AnimalData]
		val meatPart = new SimplePhysicalEntity
		meatPart.aux[FoodData].calories = 500.0f
		meatPart.aux[FlagData].flags += EntityFlag.Meat
		AD.addPart(meatPart,structural = true,NoRequirement)

		// TODO : Random
		creature.aux[SexData].sex = if (math.random < 0.5) {
			Sex.Male
		} else {
			Sex.Female
		}

		creature
	}

	protected def readResolve : Object = AnimalSpecies.withName(this.name)
	private def writeObject (stream : ObjectOutputStream) { stream.writeUTF(name) }
	private def readObject (stream : ObjectInputStream) { name = stream.readUTF() }
}





object AnimalSpecies extends TArchetypeCollector[AnimalSpecies] {
	val Sentinel : AnimalSpecies = new AnimalSpecies with TSentinel {
		override protected def readResolve : Object = AnimalSpecies.Sentinel
		override protected def createPhysicalInstance = CreatureEntity.Sentinel
	}

	protected def baseSMLLocation: String = "axis/entities/animals/Species.sml"
	protected def typeKey: String = "species"
	protected def clazz: Class[AnimalSpecies] = classOf[AnimalSpecies]
	protected def fromSML[U >: AnimalSpecies](name : String,sml: ConfigValue): U = new AnimalSpecies(sml)
}

