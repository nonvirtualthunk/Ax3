package arx.axistential.graphics.graphicsinfo

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/5/13
 * Time: 10:38 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.GeneralityLevel
import arx.core.representation.ConfigValue
import arx.tmp.game.logic.entities.core.GameArchetype
import arx.tmp.game.logic.entities.core.GameEntity

import scala.collection.mutable

abstract class ConfiguredGraphicsInfoProvider[T <: GameArchetype : Manifest](smlSourceFunc : () => Map[CaseInsensitiveString,ConfigValue]) extends TGameEntityGraphicsStructor {
	lazy val smlSource = smlSourceFunc()
	val archClass = manifest[T].runtimeClass
	def generalityLevel: GeneralityLevel.GeneralityLevel = GeneralityLevel.Inclusive

	override def graphicsInfoFor(gameEntity: GameEntity): Option[GameEntityGraphicsInfo] = {
		if ( archClass.isAssignableFrom(gameEntity.archetype.getClass) ) {
			Some(loadGraphicsInfoFor(gameEntity.archetype.asInstanceOf[T]))
		} else { None }
	}

	override def graphicsInfoFor(gameArchetype: GameArchetype): Option[GameEntityGraphicsInfo] = {
		if ( archClass.isAssignableFrom(gameArchetype.getClass) ){
			Some(loadGraphicsInfoFor(gameArchetype.asInstanceOf[T]))
		} else { None }
	}

	protected val infoByArchetype = new mutable.HashMap[T,GameEntityGraphicsInfo]()


	def loadGraphicsInfoFor( arch : T ) = {
		infoByArchetype.getOrElseUpdate(arch,subLoadGraphicsInfoFor(arch))
	}

	protected def subLoadGraphicsInfoFor( arch : T ) = {
		smlSource.get(arch.name.toLowerCase) match {
			case Some(sml) =>
				val resourceRoot = ConfigValue.extractFieldRecursive(sml,"resourceRoot").strOrElse("")
				GraphicsInfo.loadFromSML(sml,resourceRoot)
			case None => NoGraphicsInfo
		}
	}

	/**
	 * Manually register graphics info for a specific archetype. Intended for use with custom,
	 * special purpose items, at least until they are integrated into the generalized SML
	 * framework (stockpiles would likely be an example, at least early on).
	 */
	def registerGraphicsInfoFor ( arch : T , info : GameEntityGraphicsInfo ) {
		if ( infoByArchetype.contains(arch) ) { Noto.warn(s"register graphics info called with archetype that already has info ${arch.name}") }
		infoByArchetype(arch) = info
	}
}
