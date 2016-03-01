package arx.eldr.application.testbeds

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/28/16
  * Time: 1:46 PM
  */

import arx.Prelude._
import arx.core.units.UnitOfTime
import arx.engine.EngineCore
import arx.engine.advanced.Engine
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.GraphicsComponent
import arx.engine.world.World
import arx.graphics.AVBO
import arx.graphics.Texture
import arx.graphics.VBO
import arx.graphics.attributeprofiles.SimpleAttributeProfile
import arx.graphics.pov.EyeCamera
import arx.resource.ResourceManager
import scalaxy.loops._
import arx.core.vec._

object LightTestbed extends Engine {
	graphicsEngine.addComponent[BlockGraphics]

	graphicsEngine.pov = new EyeCamera(Vec3f(0,0,30),Vec3f.UnitZ * -1,Vec3f.UnitY)
}

class BlockGraphics(eng:GraphicsEngine, world : World) extends GraphicsComponent(eng, world) {
	lazy val shader = ResourceManager.shader("shaders/Simple")

	lazy val image = ResourceManager.image("default/defaultium.png")

	lazy val texture = Texture.fromImage(image)

	val vbo = new AVBO(SimpleAttributeProfile)
	
	vbo.incrementVertexOffset(4)
	vbo.incrementIndexOffset(6)

	val depth = 0.0f
	val size = 10

	val pb = SimpleAttributeProfile.createPointBuilder()
	pb.setV(-size,-size,depth)
	pb.setTC(0.0f,0.0f)
	pb.setC(1.0f,1.0f,1.0f,1.0f)
	vbo.setPoint(0,pb)

	pb.setV(size,-size,depth)
	pb.setTC(1.0f,0.0f)
	vbo.setPoint(1,pb)

	pb.setV(size,size,depth)
	pb.setTC(1.0f,1.0f)
	vbo.setPoint(2,pb)

	pb.setV(-size,size,depth)
	pb.setTC(0.0f,1.0f)
	vbo.setPoint(3,pb)

	vbo.setIQuad(0,0)
	vbo.state.set(VBO.Updated)

	override def draw(): Unit = {
		shader.bind()
		pov.look()

		texture.mipmap = false
		texture.bind()

		vbo.solidifyIfNecessary()
		vbo.drawElements()
	}
	override protected def update(dt: UnitOfTime): Unit = {

	}
}