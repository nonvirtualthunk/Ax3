package arx.axistential.game.logic.requirements

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/21/13
 * Time: 3:20 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.representation.ConfigList
import arx.core.representation.ConfigValue
import arx.tmp.game.logic.descriptors._
import arx.tmp.game.logic.entities.core.GameEntity
import arx.requirements.NumberOfEntitiesRequirement
import arx.requirements.NumberOfMatchingRequirement
import arx.requirements.TRequirement

object ConfiguredRequirement {
	protected def parseSML(obj: ConfigValue): TRequirement = {
		val amount = obj.amount.intOrElse(1)
		val descriptor = ConfiguredDescriptor(obj)
		new NumberOfMatchingRequirement(descriptor,amount)
	}

	protected def parseArray(arr: ConfigList): TRequirement = {
		val number = arr(0).float
		val kind = arr(1)

		val descriptor = ConfiguredDescriptor(kind)
		descriptor match {
			case entityDescriptor : TEntityDescriptor => new NumberOfEntitiesRequirement(entityDescriptor,number.toInt)
			case desc : TDescriptor => new NumberOfMatchingRequirement(descriptor,number.toInt)
			case _ => Noto.warn("Parsed out a non entity descriptor, not sure what to make of it"); TRequirement.Sentinel
		}
	}

	protected def parseString (str : String) : TRequirement = {
		if (str.isEmpty) {
			Noto.warn("Empty string provided as a Requirement, invalid"); TRequirement.Sentinel
		} else {
			if (str.charAt(0).isDigit) {
				val amount = str.takeWhile(_.isDigit).toInt
				val descriptor = ConfiguredDescriptor.fromString(str.takeRightWhile(! _.isDigit).trim)
				new NumberOfMatchingRequirement(descriptor,amount)
			} else {
				val descriptor = ConfiguredDescriptor.fromString(str)
				new NumberOfMatchingRequirement(descriptor,1)
			}
		}
	}

	def apply (str : String) : TRequirement = parseString(str)
	def apply (sml : ConfigValue) : TRequirement = fromSML(sml)
	def fromSML ( value : ConfigValue ) : TRequirement = {
		if ( value.isArr ) {
			parseArray(value.arr)
		} else if ( value.isObj ) {
			parseSML(value)
		} else if ( value.isStr ) {
			parseString(value.str)
		} else {
				Noto.warn("internal value was neither array nor object, cannot parse to a requirement")
				new TRequirement {
					def descriptor: TDescriptor = AnyEntityDescriptor
					def amount: Float = 0
					def amountUnitName: String = "no description"
					def amountSatisfiedBy(entity: GameEntity): Float = 0
				}
		}
	}
}

