package arx.tmp.game.logic.coordinates

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/10/13
 * Time: 4:19 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.core.Moddable
import arx.core.traits.TSentinel
import arx.core.traits.TSentinelable
import arx.core.units.UniverseUnit
import arx.core.vec.ReadVec3i

class UniverseCoord(xi:Int,yi:Int,zi:Int) extends ReadVec3i(xi:Int,yi:Int,zi:Int) with TSentinelable {
	def this() { this(0,0,0) }
	
	override def + ( mv : Moddable[ReadVec3i] ) : UniverseCoord = { val v = mv.resolve(); new UniverseCoord(x + v.x,y + v.y,z + v.z) }
	override def - ( mv : Moddable[ReadVec3i] ) : UniverseCoord = { val v = mv.resolve(); new UniverseCoord(x - v.x,y - v.y,z - v.z) }

	override def + ( v : ReadVec3i ) : UniverseCoord = { new UniverseCoord(x + v.x,y + v.y,z + v.z) }
	override def - ( v : ReadVec3i ) : UniverseCoord = { new UniverseCoord(x - v.x,y - v.y,z - v.z) }
	override def / ( f : Int ) : UniverseCoord = { new UniverseCoord(x/f,y/f,z/f) }
	override def * ( f : Int ) : UniverseCoord = { new UniverseCoord(x*f,y*f,z*f) }

	def plusZ ( f : Int ) : UniverseCoord = { new UniverseCoord(x,y,z + f) }
	def minusZ ( f : Int ) : UniverseCoord = { new UniverseCoord(x,y,z - f) }
	def plusX ( f : Int ) : UniverseCoord = { new UniverseCoord(x+f,y,z) }
	def minusX ( f : Int ) : UniverseCoord = { new UniverseCoord(x-f,y,z) }
	def plusY ( f : Int ) : UniverseCoord = { new UniverseCoord(x,y+f,z) }
	def minusY ( f : Int ) : UniverseCoord = { new UniverseCoord(x,y-f,z) }

	def distanceTo ( u : UniverseCoord ) = {
		val dx = u.x - this.x
		val dy = u.y - this.y
		val dz = u.z - this.z
		val euc = dx * dx + dy * dy + dz * dz
		if ( euc != 0 ) {
			sqrtf(euc).uu
		} else { zeroMeters }
	}
}

object UniverseCoord {
	val Sentinel : UniverseCoord = new UniverseCoord with TSentinel
	val Max = UniverseCoord(Int.MaxValue,Int.MaxValue,Int.MaxValue)
	val Min = UniverseCoord(Int.MinValue,Int.MinValue,Int.MinValue)
	val Zero = UniverseCoord(0,0,0)

	def apply ( x : Int ,y : Int, z : Int ) = new UniverseCoord(x,y,z)
	def apply ( v : ReadVec3i ) = new UniverseCoord(v.x,v.y,v.z)
	def unapply ( c : UniverseCoord ) = Some((c.x,c.y,c.z))

	val scale = UniverseUnit.conversion.meters
}
