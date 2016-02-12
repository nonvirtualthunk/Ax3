package arx.tmp.game.logic.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/14/12
 * Time: 8:52 AM
 * Created by nonvirtualthunk
 */

import java.io.ObjectInput
import java.io.ObjectOutput

object DummyInfiniteByteVoxelStore extends TInfiniteVoxelStore[Byte] {
		def apply(x: Int, y: Int, z: Int) = 0.toByte
		def update(x: Int, y: Int, z: Int, t: Byte) {}
		def writeExternal(p1: ObjectOutput) {}
		def readExternal(p1: ObjectInput) {}
	}