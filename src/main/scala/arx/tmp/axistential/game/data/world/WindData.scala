package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/29/13
 * Time: 10:26 AM
 */

import arx.Prelude._
import arx.core.gen.ArxGenerators
import arx.core.gen.Generator
import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec2i
import arx.core.vec.Vec2i
import arx.engine.data.TWorldAuxData
import arx.tmp.game.logic.datastructures.CellGrid2D

class WindData extends TWorldAuxData {
	var windX = CellGrid2D[Float](4)
	var windY = CellGrid2D[Float](4)
//	var speedGrid = CellGrid2D[Float](2)

	var prevailingWindGenerator : Generator = ArxGenerators.ZeroGenerator
	var directionDynamicGenerator : Generator = ArxGenerators.ZeroGenerator
	var speedDynamicGenerator : Generator = ArxGenerators.IdentityGenerator

	def windVector ( x : Int , y : Int ) : ReadVec2f = {
		val ts = world.time.inSeconds
		val wx = windX(x,y)
		val wy = windY(x,y)

		val dynTheta = prevailingWindGenerator(ts) + directionDynamicGenerator(x,y,ts)
		val dynSpeed = speedDynamicGenerator(x,y,ts)
		ReadVec2f(wx + cosf(dynTheta) * dynSpeed,wy + sinf(dynTheta) * dynSpeed)
	}
	def setWindVector ( x : Int , y : Int , dx : Float , dy : Float ) {
		windX(x,y) = dx
		windY(x,y) = dy
	}

	def prevailingWindDirection = prevailingWindGenerator(world.time.inSeconds)
}

object WindData {
	@inline final def packWindVector ( dx : Int , dy : Int ) : Short = (( dx << 8 ) + dy).toShort
	@inline final def unpackWindVector ( s : Short ) : ReadVec2i = ReadVec2i( s >> 8 , s & 0x00ff )
	@inline final def unpackWindVector ( s : Short , v : Vec2i ) {
		v.x = s >> 8
		v.y = s & 0x00ff
	}
}



//class WindGameComponent extends GameEngineComponent {
//	def update(time: UnitOfTime): Unit = {
//
//	}
//}
