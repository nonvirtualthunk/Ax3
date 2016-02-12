package arx.tmp.game.logic.lighting

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/21/13
 * Time: 8:33 AM
 * Created by nonvirtualthunk
 */

import java.io.Externalizable
import java.io.ObjectInput
import java.io.ObjectOutput

import arx.Prelude._
import arx.engine.data.TWorldAuxData
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.entities.TLightSource

import scala.collection.mutable

class PerLightData extends TWorldAuxData with Externalizable {
	//Replace with octant based structure
	//Or, at very least something fast
	val shadowGrids = new mutable.WeakHashMap[TLightSource,TShadowGrid]
	val intensityFalloffGrid = new ShadowGrid//new GenericRawTaleaGrid[Float,GenericTalea[Float]]((v:Vec3i) => new GenericTalea[Float](VoxelCoord(v),0.toByte))

	def createShadowGridFor(ent:TLightSource) = if ( ent.lightStrength < 32 ) {
		new OctantShadowGrid
	} else {
		new ShadowGrid//new GenericRawTaleaGrid[Float,GenericTalea[Float]]((v:Vec3i) => new GenericTalea[Float](VoxelCoord(v),0.0f))
	}

	def shadowGridFor ( ent : TLightSource ) = {
		shadowGrids.getOrElseUpdate(ent,createShadowGridFor(ent))
	}
	def removeShadowGridFor ( ent : TLightSource ) { shadowGrids.remove(ent) }

//	val centerX = FancyLocalLightComputor.centerX
//	val centerY = FancyLocalLightComputor.centerY
//	val centerZ = FancyLocalLightComputor.centerZ
//	val center = FancyLocalLightComputor.center
	val centerX = 32 * 5
	val centerY = 32 * 5
	val centerZ = 32 * 5
	val center = VoxelCoord(centerX,centerY,centerZ)

//	for ( x <- centerX - 64 to centerX + 64 ; y <- centerX - 64 to centerX + 64 ; z <- centerX - 64 to centerX + 64 ) {
	for ( x <- -63 to 63 ; y <- -63 to 63 ; z <- -63 to 63 ) {
		intensityFalloffGrid(x,y,z) = distance(x,y,z,center)
	}

	def writeExternal(out: ObjectOutput) {}
	def readExternal(in: ObjectInput) {}
}
