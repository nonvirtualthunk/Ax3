package arx.axistential.game.data.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/10/15
 * Time: 2:55 PM
 */

import arx.Prelude._
import arx.axistential.game.archetypes.species.Species
import arx.axistential.game.data.entity.AnimalAIData.Patch
import arx.core.datastructures.CountMap
import arx.core.representation.ConfigValue
import arx.core.representation.InformationLevel.InformationLevel
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.TMajorCoord
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion
import arx.tmp.game.logic.entities.core._
import arx.tmp.game.logic.entities.data.TAuxData
import arx.tmp.game.logic.entities.data.TConfigurableGameEntityAuxData
import arx.tmp.game.logic.entities.data.TGameEntityAuxData
import arx.tmp.game.logic.entities.data.TInheritableAuxData

@SerialVersionUID(1L)
class AnimalAIData extends TGameEntityAuxData with TConfigurableGameEntityAuxData with TInheritableAuxData {
	var homePatch : Option[Patch] = None
	var patchSize : Int = 15

	var patches = List[AnimalAIData.Patch]()
	/** The last time (in world-time) when a search for a new patch found nothing usable. Used to limit new-patch searches */
	var lastPatchSearchFailure = Option.empty[UnitOfTime]

	def patchFor(coord: TMajorCoord, patchRadius : Int) = {
		patchForIfExists(coord) match {
			case Some(p) => p
			case None =>
				val newPatch = Patch(VoxelRegion(coord,patchRadius))
				patches ::= newPatch
				newPatch
		}
	}

	def patchForIfExists(coord : TMajorCoord) = {
		val possiblePatches = patches.filter(p => p.region.contains(coord.toVoxelCoord))
		possiblePatches match {
			case Nil => {
				None
			}
			case list =>
				Some(list.minBy(p => p.region.center.distanceTo(coord)))
		}
	}


	override def createFromSML(sml: ConfigValue): Option[AnimalAIData] = {
		if (sml.patchSize.nonEmpty) {
			val ret = new AnimalAIData
			ret.patchSize = sml.patchSize.int
			Some(ret)
		} else {
			None
		}
	}

	override def informationPairs(level: InformationLevel): Map[String, Any] = Map()
}

object AnimalAIData {
	case class Patch (region : VoxelRegion) extends THasAuxData[TPatchData] {
		def forage = aux[ForageData]


		var pathCostTo = Map[Patch,Float]()
		var lastArrivedAt = 0.seconds
	}


	trait TPatchData extends Serializable with TAuxData {
		def patchEntered (time : UnitOfTime) {}
		def patchExited (time : UnitOfTime) {}
	}


	class DecayingRecord[T] (newZeroValue : => T, clearValue : (T) => T, merge : (T,T) => T) extends Serializable {
		var scratch : T = newZeroValue
		var record : Option[T] = None

		def finishAndUpdate (): Unit = {
			record = record match {
				case Some(hc) => Some(merge(hc,scratch))
				case None => Some(scratch)
			}
			scratch = clearValue(scratch)
		}
	}
	class DecayingFloatRecord extends DecayingRecord[Float] (
		0.0f,
		(f) => 0.0f,
		(old,n) => if (old <= 0.0f && n <= 0.001f) { old - 1.0f } else { (old + n) * 0.5f }
	)

	class DecayingCountMapRecord[U] extends DecayingRecord[CountMap[U]](new CountMap[U],(m) => {m.clear(); m},(old,n) => {
		val mergedKeys = old.keySet ++ n.keySet
		for (k <- mergedKeys) {
			old(k) = old.get(k) match {
				case None => n(k)
				case Some(v) => (v + n(k)) * 0.5f
			}
		}
		old
	})

	class OtherCreatureData extends TPatchData {
		val observedCreatures = new DecayingCountMapRecord[Species]
		/** Historical decaying record of the species encountered in this patch. Each set of observations
		  * are averaged against the current history to give the new value. */
		def historicalObservedCreatures = observedCreatures.record
		/** Scratch variable for the creatures observed in this patch at a given time */
		def currentObservedCreatures = observedCreatures.scratch
		def creatureObserved (s : Species) { observedCreatures.scratch.increment(s,1.0f) }


		override def patchExited (time : UnitOfTime): Unit = {
			observedCreatures.finishAndUpdate()
		}
	}

	class ForageData extends TPatchData {
		protected var forageCalories = new DecayingFloatRecord
		/** How many calories we can tentatively expect from this patch, based on what
		  * we've seen in the past, if any. Uses an exponentially decaying method, each
		  * new value is averaged with the existing to get the new one */
		def historicalForageCalories = forageCalories.record
		/** Scratch variable for keeping track of the calories foraged in the current
		  * foraging attempt */
		def currentForageCalories = forageCalories.scratch
		def currentForageCalories_= (f : Float) { forageCalories.scratch = f }
		/** When last this patch was visited */
		var lastForaged = 0.seconds


		/** Update the exponentially decaying average historical calories based on the current
		  * forage calories, then reset that to zero. */
		def finishForageAndUpdateExpectedCalories(currentTime : UnitOfTime) = {
			forageCalories.finishAndUpdate()
			lastForaged = currentTime
		}
	}

	class HuntData extends TPatchData {
		var huntCalories = new DecayingFloatRecord
		var seenCalories = new DecayingFloatRecord
		/** How many calories we can tentatively expect from this patch, based on what
		  * we've caught in the past, if any. Uses an exponentially decaying method, each
		  * new value is averaged with the existing to get the new one */
		def historicalHuntCalories = huntCalories.record
		/** How many calories we can tentatively expect to see in this patch, regardless of
		  * actual success. If there is a large quantity of prey in a given area, but we have
		  * not had good luck in catching it, we want to know that */
		def historicalSeenCalories = seenCalories.record

		/** Scratch variable for keeping track of the calories hunted in the current
		  * hunting attempt */
		def currentHuntCalories = huntCalories.scratch
		def currentHuntCalories_=(f:Float){ huntCalories.scratch = f }

		/** Scratch variable for keeping track of the calories seen in the current
		  * hunting attempt */
		def currentSeenCalories = seenCalories.scratch
		def currentSeenCalories_=(f:Float){ seenCalories.scratch = f }

		/** When last this patch was visited */
		var lastHunted = 0.seconds


		/** Update the exponentially decaying average historical calories based on the current
		  * forage calories, then reset that to zero. */
		def finishForageAndUpdateExpectedCalories(currentTime : UnitOfTime) = {
			huntCalories.finishAndUpdate()
			seenCalories.finishAndUpdate()

			currentHuntCalories = 0.0f
			lastHunted = currentTime
		}
	}
}


class BurrowType(nomen : String) extends ArxEnum(nomen : String) {}

object BurrowType extends ArxEnumObject[BurrowType] {
	val TreeNest = BurrowType("TreeNest")
	val TreeBole = BurrowType("TreeBole")
	val Hole = BurrowType("Hole")
	val Warren = BurrowType("Warren")
	val None = BurrowType("None")
}

class AnimalGroupType (noment : String) extends ArxEnum(noment : String)

object AnimalGroupType extends ArxEnumObject[AnimalGroupType] {
	val Pack = AnimalGroupType("Pack")
	val Herd = AnimalGroupType("Herd")
	val Solitary = AnimalGroupType("Solitary")

}