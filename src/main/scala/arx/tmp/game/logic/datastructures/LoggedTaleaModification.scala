package arx.tmp.game.logic.datastructures

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/24/15
 * Time: 8:43 PM
 */

import arx.core.vec.ReadVec3i

object LoggedTaleaModification {
	def apply[T] (v : ReadVec3i, ov : T, nv : T, rev : Int) = {
		new LoggedTaleaModification[T](v,ov,nv,rev)
	}
	def apply[T] (x : Int, y : Int, z : Int, ov : T, nv : T, rev : Int) = {
		new LoggedTaleaModification[T](x.toByte,y.toByte,z.toByte,ov,nv,rev)
	}
}

class LoggedTaleaModification[@specialized(Byte,Short,Int) T] (val x : Byte,val y : Byte,val z : Byte,val oldValue : T ,val newValue : T ,val revision : Int ) {
	def this (v : ReadVec3i, ov : T, nv : T, rev : Int) {
		this(v.x.toByte,v.y.toByte,v.z.toByte,ov,nv,rev)
	}
	def position = ReadVec3i(x,y,z)
}

//class ByteLoggedTaleaModificationSerializer extends ArxKryoSerializer[LoggedTaleaModification[Byte]] {
//	override def write(kryo: Kryo, output: Output, t: LoggedTaleaModification[Byte]): Unit = {
//
//	}
//	override def read(kryo: Kryo, input: Input, aClass: Class[LoggedTaleaModification[Byte]]): LoggedTaleaModification[T] = {
//
//	}
//}