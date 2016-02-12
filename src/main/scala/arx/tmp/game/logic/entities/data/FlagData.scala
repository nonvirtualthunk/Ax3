package arx.tmp.game.logic.entities.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/8/14
 * Time: 9:21 AM
 */

import arx.core.representation.ConfigValue
import arx.core.representation.InformationLevel.InformationLevel
import arx.tmp.game.logic.entities.core.GameEntity

import scala.collection.mutable

class FlagData extends TConfigurableGameEntityAuxData with TInheritableAuxData {
	protected var _flags = Set[EntityFlag]()
	def flags = _flags
	def flags_= (fls : Set[EntityFlag]) = {
		val s = new mutable.HashSet[EntityFlag]
		for (f <- fls) {
			recursivelyExpand(f,s)
		}
		_flags = s.toSet
	}

	protected def recursivelyExpand (f : EntityFlag, ret : mutable.HashSet[EntityFlag]) : Unit = {
		ret.add(f)
		for (s <- f.subsetOf) {
			if (! ret.contains(s)) {
				recursivelyExpand(s,ret)
			}
		}
	}

	override def copyToInstance(entity: GameEntity): Unit = {
		val FD = entity.aux[FlagData]
		FD.flags = flags
	}
	override def createFromSML(sml: ConfigValue): Option[FlagData] = {
		if (sml.flags.nonEmpty) {
			val FD = new FlagData
			sml.flags.arr.foreach(sf => FD.flags += EntityFlag.fromString(sf.str))
			Some(FD)
		} else {
			None
		}
	}

	override def informationPairs(level: InformationLevel): Map[String, Any] = Map()
}
