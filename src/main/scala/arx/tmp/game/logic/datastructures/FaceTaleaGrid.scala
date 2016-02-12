package arx.tmp.game.logic.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/25/12
 * Time: 8:04 PM
 * Created by nonvirtualthunk
 */

import java.io.Externalizable

import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.coordinates.VoxelSideCoord

class FaceTaleaGrid extends GenericTaleaGrid[Int,FaceTalea](0,(v:VoxelCoord) => new FaceTalea(v)) with Externalizable {

	def apply(loc : VoxelSideCoord) : Byte = {
		apply(loc.voxel.x,loc.voxel.y,loc.voxel.z,loc.side)
	}
	def apply(x: Int, y: Int, z: Int, q : Int) : Byte = {
		if ( q < 3 ) {
			forSide( apply(x,y,z) , q ).toByte
		} else if ( q == 3 ) {
			apply(x+1,y,z,q-3)
		} else if ( q == 4 ) {
			apply(x,y+1,z,q-3)
		} else if ( q == 5 ) {
			apply(x,y,z+1,q-3)
		} else { 0 }
	}

	def update(loc : VoxelSideCoord, newValue : Byte): Unit = {
		update(loc.voxel.x,loc.voxel.y,loc.voxel.z,loc.side,newValue)
	}
	def update(x: Int, y: Int, z: Int, q: Int, newValue : Byte) {
		if ( q < 3 ) {
			val talea = taleaFor(x,y,z)

			val cur = talea(x - talea.x,y - talea.y,z - talea.z)
			talea(x - talea.x,y - talea.y,z - talea.z) = setForSide(cur,q,newValue)
		} else if ( q == 3 ) {
			update(x+1,y,z,q-3,newValue)
		} else if ( q == 4 ) {
			update(x,y+1,z,q-3,newValue)
		} else if ( q == 5 ) {
			update(x,y,z+1,q-3,newValue)
		}
	}


	def faceWindowCenteredOnTaleaAt(center: VoxelCoord, readOnly: Boolean) : FaceTaleaGridWindow = {
		new FaceTaleaGridWindow(this,(center >> Talea.dimensionPo2) << Talea.dimensionPo2,readOnly)
	}

	def forSide ( currentValue : Int, q : Int ) = (currentValue >> (q << 3)) & 0x000000ff
	def setForSide ( currentValue : Int, q : Int , sideValue : Int ) = {
		(currentValue & (~(0x000000ff << (q << 3)))) | (sideValue << (q << 3))
	}
}

class FaceTalea(v: VoxelCoord) extends GenericTalea[Int](v,0) {
	// ignore default value, we always use 0
	def this (pos : VoxelCoord, defVal : Int) { this(pos) }

	def apply(x: Int, y: Int, z: Int,q: Int) : Byte = {
		if ( q < 3 ) {
			forSide(apply(x,y,z),q)
		}
		else if ( q == 3 ) { apply(x+1,y,z,q-3) }
		else if ( q == 4 ) { apply(x,y+1,z,q-3) }
		else if ( q == 5 ) { apply(x,y,z+1,q-3) }
		else { 0.toByte }
	}
	def update(x: Int, y: Int, z: Int, q:Int, b: Byte) {
		if ( q < 3 ) {
			val cur = apply(x,y,z)
			update(x,y,z,setForSide(cur,q,b))
		}
		else if ( q == 3 ) { update(x+1,y,z,q-3,b) }
		else if ( q == 4 ) { update(x,y+1,z,q-3,b) }
		else if ( q == 5 ) { update(x,y,z+1,q-3,b) }
	}

	def forSide ( currentValue : Int, q : Int ) : Byte = ((currentValue >> (q << 3)) & 0x000000ff).toByte
	def setForSide ( currentValue : Int, q : Int , sideValue : Int ) = {
		(currentValue & (~(0x000000ff << (q << 3)))) | (sideValue << (q << 3))
	}
}

class FaceTaleaGridWindow(grid : FaceTaleaGrid,center : VoxelCoord,readOnly : Boolean) extends GenericTaleaGridWindow[Int,FaceTalea](grid,center,readOnly) {
	def apply (x : Int, y : Int, z : Int, q : Int ) : Byte = {
		if ( q < 3 ) {
			forSide(apply(x,y,z),q)
		}
		else if ( q == 3 ) { apply(x+1,y,z,q-3) }
		else if ( q == 4 ) { apply(x,y+1,z,q-3) }
		else if ( q == 5 ) { apply(x,y,z+1,q-3) }
		else { throw new IllegalStateException("invalid face index : " + q) }
	}
	def update(x: Int, y: Int, z: Int, q:Int, b: Byte) {
		if ( q < 3 ) {
			val cur = apply(x,y,z)
			update(x,y,z,setForSide(cur,q,b))
		}
		else if ( q == 3 ) { update(x+1,y,z,q-3,b) }
		else if ( q == 4 ) { update(x,y+1,z,q-3,b) }
		else if ( q == 5 ) { update(x,y,z+1,q-3,b) }
	}

	def forSide ( currentValue : Int, q : Int ) : Byte = ((currentValue >> (q << 3)) & 0x000000ff).toByte
	def setForSide ( currentValue : Int, q : Int , sideValue : Int ) = {
		(currentValue & (~(0x000000ff << (q << 3)))) | (sideValue << (q << 3))
	}
}

