package arx.tmp.game.logic.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/11/12
 * Time: 8:20 AM
 * Created by nonvirtualthunk
 */

import java.io.ObjectInput
import java.io.ObjectOutput

import com.carrotsearch.hppc.LongByteOpenHashMap
import com.carrotsearch.hppc.LongObjectOpenHashMap

class HashVoxelStore[T](var defaultValue:T) extends TInfiniteVoxelStore[T] {
	def this () { this(null.asInstanceOf[T]) }
	var map = new LongObjectOpenHashMap[T]

	def hash(x:Int,y:Int,z:Int) = (x << 40) + (y << 20) + z
	def apply(x: Int, y: Int, z: Int) = {
		val base = map.get(hash(x,y,z))
		if ( base == null ) { defaultValue }
		else { base }
	}
	def update(x: Int, y: Int, z: Int, t: T) { map.put(hash(x,y,z),t) }

	def writeExternal(p1: ObjectOutput) {p1.writeObject(defaultValue);p1.writeObject(map);}
	def readExternal(p1: ObjectInput) {defaultValue = p1.readObject.asInstanceOf[T];map = p1.readObject.asInstanceOf[LongObjectOpenHashMap[T]]}
}

class HashVoxelByteStore(var defaultValue:Byte) extends TInfiniteVoxelStore[Byte] {
	def this () { this(0.toByte) }
	var map = new LongByteOpenHashMap()

	def hash(x:Int,y:Int,z:Int) = (x << 40) + (y << 20) + z
	def apply(x: Int, y: Int, z: Int) = {
		if ( map.containsKey(hash(x,y,z)) ) {
			map.lget()
		} else {
			defaultValue
		}
	}
	def update(x: Int, y: Int, z: Int, t: Byte) { map.put(hash(x,y,z),t) }

	def writeExternal(p1: ObjectOutput) {p1.writeByte(defaultValue);p1.writeObject(map);}
	def readExternal(p1: ObjectInput) {defaultValue = p1.readByte;map = p1.readObject.asInstanceOf[LongByteOpenHashMap]}
}