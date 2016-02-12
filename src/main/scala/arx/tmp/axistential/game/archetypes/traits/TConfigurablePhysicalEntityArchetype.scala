package arx.axistential.game.archetypes.traits

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/8/13
 * Time: 9:25 AM
 */

import arx.application.Noto
import arx.axistential.game.logic.physics.CollisionShape
import arx.axistential.game.logic.physics.CubeCollisionShape
import arx.core.representation.ConfigValue
import arx.core.representation.TConfigurable
import arx.core.units.Dimensions
import arx.core.units.UnitOfMeasure

trait TConfigurablePhysicalEntityArchetype extends TPhysicalEntityArchetype with TConfigurable {
	def parseCollisionShape ( sml : ConfigValue , defaultShape : String = "none" ) = sml.shape.strOrElse(defaultShape) match {
		case "box" => {
			val dims = parseDimensions(sml)
			new CubeCollisionShape(dims)
		}
		case "none" => {
			Noto.warn(s"Item archetype with not collision shape....that's not ideal : $name")
			CollisionShape.Sentinel
		}
	}

	/**
	 * Parse out the dimensions of the given sml object. Note: this should
	 * be the overall object, _not_ the .dimensions field
	 */
	def parseDimensions ( sml : ConfigValue ) = {
		if ( sml.dimensions.isArr ) {
			val arr = sml.dimensions.arr
			if ( arr.size == 3 ){
				val x = UnitOfMeasure.parseUnitOfDistance(arr(0).strOrElse(""))
				val y = UnitOfMeasure.parseUnitOfDistance(arr(1).strOrElse(""))
				val z = UnitOfMeasure.parseUnitOfDistance(arr(2).strOrElse(""))

				new Dimensions(x,y,z)
			} else {
				Noto.warn("dimensions provided as an array, but did not have 3 elements")
				Dimensions.Zero
			}
		} else if ( sml.dimensions.isObj ) {
			val obj = sml.dimensions
			val x = UnitOfMeasure.parseUnitOfDistance(obj.field("x").strOrElse(""))
			val y = UnitOfMeasure.parseUnitOfDistance(obj.y.strOrElse(""))
			val z = UnitOfMeasure.parseUnitOfDistance(obj.z.strOrElse(""))

			new Dimensions(x,y,z)
		} else {
			Noto.warn(s"sml.dimensions had unacceptable datatype: ${sml.dimensions}")
			Dimensions.Zero
		}
	}
}
