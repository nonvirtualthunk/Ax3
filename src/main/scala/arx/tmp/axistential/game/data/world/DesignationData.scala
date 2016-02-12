package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/14/13
 * Time: 4:38 PM
 */
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TWorldAuxData
import arx.tmp.game.logic.datastructures.GenericRawTaleaGrid
import arx.tmp.game.logic.datastructures.GenericTalea

@SerialVersionUID(1L)
class DesignationData extends TWorldAuxData with Serializable {
	val removalByte = -1.toByte
	val additionByte = 1.toByte
	val changeByte = 2.toByte
	val immediateAdditionByte = 4.toByte
	var designations = new GenericRawTaleaGrid[Byte,GenericTalea[Byte]](0,(v:VoxelCoord) => new GenericTalea[Byte] (v,0) )

	def isDesignatedForRemoval(x:Int,y:Int,z:Int) = designations(x,y,z) == removalByte
	def isDesignatedForAddition(x:Int,y:Int,z:Int) = designations(x,y,z) == additionByte
	def isDesignatedForChange(x:Int,y:Int,z:Int) = designations(x,y,z) == changeByte
	def isDesignatedForImmediateAddition(x:Int,y:Int,z:Int) = designations(x,y,z) == immediateAdditionByte
	def noDesignationAt(x:Int,y:Int,z:Int) = designations(x,y,z) == 0.toByte
	def anyDesignationAt(x:Int,y:Int,z:Int) = designations(x,y,z) != 0.toByte
	def designationAt (x:Int,y:Int,z:Int) = { designations(x,y,z) }
	def designateForRemoval(x:Int,y:Int,z:Int) { designations(x,y,z) = removalByte }
	def designateForAddition(x:Int,y:Int,z:Int) { designations(x,y,z) = additionByte }
	def designateForImmediateAddition(x:Int,y:Int,z:Int) { designations(x,y,z) = immediateAdditionByte }
	def designateForChange(x:Int,y:Int,z:Int) { designations(x,y,z) = changeByte }
	def clearDesignation(x:Int,y:Int,z:Int) { designations(x,y,z)= 0.toByte }
	def clearRemovalDesignation(x:Int,y:Int,z:Int) = { designations.setIfEqual(x,y,z,removalByte,0.toByte) == removalByte }
	def clearAdditionDesignation(x:Int,y:Int,z:Int) = { designations.setIfEqual(x,y,z,additionByte,0.toByte) == additionByte }
	def clearImmediateAdditionDesignation(x:Int,y:Int,z:Int) = { designations.setIfEqual(x,y,z,additionByte,0.toByte) == immediateAdditionByte }
}

object DummyDesignationData extends DesignationData {
	override def isDesignatedForRemoval(x:Int,y:Int,z:Int) = false
	override def isDesignatedForAddition(x:Int,y:Int,z:Int) = false
	override def isDesignatedForChange(x: Int, y: Int, z: Int) = false
	override def isDesignatedForImmediateAddition(x: Int, y: Int, z: Int) = false

	override def designateForRemoval(x: Int, y: Int, z: Int){}
	override def designateForAddition(x: Int, y: Int, z: Int){}
	override def designateForChange(x: Int, y: Int, z: Int){}
	override def designateForImmediateAddition(x: Int, y: Int, z: Int){}
}