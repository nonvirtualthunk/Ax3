package arx.tmp.game.logic.entities.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/30/14
 * Time: 3:29 PM
 */

import arx.application.Noto
import arx.core.traits.ArxEnum
import arx.core.traits.TArxEnum
import arx.resource.ResourceManager

class EntityFlag(nomen : String) extends ArxEnum(nomen.toLowerCase.filterNot(_.isWhitespace)) {
	var description = nomen 
	def withDescription (desc : String) = { description = desc ; this }
	def withSubsetOf(other : EntityFlag) = { subsetOf += other }

	var subsetOf : Set[EntityFlag] = Set()
}

object EntityFlag {
	private val allFlags = {
		var ret = Map[String,EntityFlag]()
		val sml = ResourceManager.sml("axis/entities/flags/EntityFlags.sml")
		for ((namespace,v) <- sml.fields) {
			for ((name,flagData) <- v.flags.fields) {
				val flag = new EntityFlag(name).withDescription(flagData.description.str)
				ret += name -> flag
			}
		}

		for ((namespace,v) <- sml.fields) {
			for ((name,flagData) <- v.flags.fields) {
				val flag = ret(name)
				for (subsetKey <- flagData.subsetOf.arr) {
					if (ret.contains(subsetKey.str)) {
						flag.withSubsetOf(ret(subsetKey.str))
					} else {
						Noto.warn(s"Invalid subsetOf provided to flag, $name, subsetOf : ${subsetKey.str}")
					}
				}
			}
		}
		ret
	}

	val Edge = EntityFlag("Edge")
	val SharpEdge = EntityFlag("SharpEdge")
	val DurableEdge = EntityFlag("DurableEdge")
	val DurablePoint = EntityFlag("DurablePoint")
	val BasicCraftingTable = EntityFlag("BasicCraftingTable")
	val Quern = EntityFlag("Quern")
	val Meat = EntityFlag("Meat")

	val UnknownFlag = new EntityFlag("unknown flag").withDescription("unknown flag")

	def apply (str : String) = fromString(str)
	def fromString (str : String) = {
		val key = str.toLowerCase.filterNot(_.isWhitespace)
		TArxEnum.withKey[EntityFlag](key,UnknownFlag)
	}
	def fromStringOpt (str : String) = {
		val key = str.toLowerCase.filterNot(_.isWhitespace)
		TArxEnum.existing[EntityFlag](key)
	}
}