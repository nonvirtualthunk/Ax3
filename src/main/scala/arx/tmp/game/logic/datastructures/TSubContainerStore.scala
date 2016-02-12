package arx.tmp.game.logic.datastructures

import arx.core.vec.coordinates.VoxelCoord

import scala.collection.mutable.HashMap

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 11/12/11
 * Time: 1:56 PM
 * Created by nonvirtualthunk
 */

trait TSubContainerStore[+T] {
	def getSubContainer ( x : Int , y : Int , z : Int) : T
	def subContainerDimension : Int
	def subContainerDimensionPo2 : Int
}

/* Not Thread Safe */
class GenericSubContainerStore[T](val subContainerDimensionPo2 : Int,creationFunc : (VoxelCoord) => T ) extends TSubContainerStore[T] {
	val subContainers = new HashMap[Int,T]()
	def getSubContainer ( x : Int , y : Int , z : Int) : T = {
		subContainers.getOrElseUpdate(hash(x,y,z),creationFunc(VoxelCoord(x,y,z)))
	}
	def apply ( x : Int, y : Int, z : Int ) : T = getSubContainer(x,y,z)
	val subContainerDimension : Int = 1 << subContainerDimensionPo2

	def hash ( x : Int, y : Int , z : Int ) = ((x >> subContainerDimensionPo2) << 20) + ((y >> subContainerDimensionPo2) << 10) + (z >> subContainerDimensionPo2)
}