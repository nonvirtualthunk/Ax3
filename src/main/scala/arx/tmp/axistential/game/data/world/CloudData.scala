package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/13
 * Time: 11:59 AM
 * To change this template use File | Settings | File Templates.
 */

import java.io.Externalizable
import java.io.ObjectInput
import java.io.ObjectOutput

import arx.engine.data.TWorldAuxData
import arx.graphics.Image

@SerialVersionUID(1L)
class CloudData extends TWorldAuxData with Externalizable {
	var cloudOffset : Float = 0.0f
	var cloudImage : Image = Image.Sentinel

	def writeExternal(out: ObjectOutput) {
		out.writeFloat(cloudOffset)
	}
	def readExternal(in: ObjectInput) {
		cloudOffset = in.readFloat
	}
}