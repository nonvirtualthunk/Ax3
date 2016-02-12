package arx.tmp.game.networking

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/24/15
 * Time: 8:40 PM
 */

import arx.core.datastructures.primitive.Primitive
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.LoggedTaleaModification
import arx.tmp.game.logic.world.data.NetworkedWorldDataUpdate
import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output

import scalaxy.loops._

case class GridDelta[T] (gridId : Int,taleaPosition : VoxelCoord, modifications : Seq[LoggedTaleaModification[T]])(implicit man : Manifest[T]) extends NetworkedWorldDataUpdate(gridId) {
	def primitiveTypeIndex = Primitive.primId(manifest[T])
}

class GridDeltaSerializer extends ArxKryoSerializer[GridDelta[_]] {
	override def write(kryo: Kryo, output: Output, t: GridDelta[_]): Unit = {
		val primId = t.primitiveTypeIndex
		output.writeInt(primId)
		output.writeInt(t.gridId)
		kryo.writeObject(output,t.taleaPosition)
		output.writeShort(t.modifications.size)
		for (mod <- t.modifications) {
			output.writeByte(mod.x)
			output.writeByte(mod.y)
			output.writeByte(mod.z)
			Primitive.writePrim(output,primId,mod.oldValue)
			Primitive.writePrim(output,primId,mod.newValue)
		}
		kryo.writeObject(output,t.forClass)
	}
	override def read(kryo: Kryo, input: Input, aClass: Class[GridDelta[_]]): GridDelta[_] = {
		val primId = input.readInt
		val gridId = input.readInt
		val taleaPos = kryo.readObject(input,classOf[VoxelCoord])
		val numMods = input.readShort
		var mods = Vector[LoggedTaleaModification[AnyVal]]()
		for (i <- 0 until numMods optimized) {
			val x = input.readByte
			val y = input.readByte
			val z = input.readByte
			val oldV = Primitive.readPrim(input,primId)
			val newV = Primitive.readPrim(input,primId)
			mods :+= new LoggedTaleaModification[AnyVal](x,y,z,oldV,newV,1)
		}
		val forClass = kryo.readObject(input,classOf[Class[_]])
		val ret = GridDelta[AnyVal](gridId,taleaPos,mods)(Primitive.manifestFor(primId).asInstanceOf[Manifest[AnyVal]])
		ret.forClass = forClass
		ret
	}
}
