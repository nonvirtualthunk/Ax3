package arx.rog2.game.data.world

/**
  * TODO: Add javadoc
  */

import arx.engine.data.TWorldAuxData
import arx.engine.entity.TGameEntity
import arx.modules.lighting.OctantShadowGrid
import arx.modules.lighting.ShadowGrid
import arx.modules.lighting.TShadowGrid
import arx.rog2.game.data.entity.LightSource

import scala.collection.mutable
import arx.Prelude._

class Light extends TWorldAuxData {
	val shadowGrids = new mutable.HashMap[TGameEntity,TShadowGrid]
	val lightGrids = new mutable.HashMap[TGameEntity,TShadowGrid]

	def createShadowGridFor(ent:TGameEntity) = if ( ent[LightSource].lightStrength < 32.voxels ) {
		new OctantShadowGrid
	} else {
		new ShadowGrid
	}

	def shadowGridFor ( ent : TGameEntity ) = {
		shadowGrids.getOrElseUpdate(ent,createShadowGridFor(ent))
	}
	def removeShadowGridFor ( ent : TGameEntity ) { shadowGrids.remove(ent) }

	def lightGridFor ( ent : TGameEntity ) = {
		lightGrids.synchronized {
			lightGrids.getOrElseUpdate(ent,createShadowGridFor(ent))
		}
	}
}
