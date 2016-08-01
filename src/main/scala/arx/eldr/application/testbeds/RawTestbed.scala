package arx.eldr.application.testbeds

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 7/31/16
  * Time: 3:54 PM
  */

import arx.Prelude._
import arx.engine.EngineCore
import arx.graphics.{GL, TextureBlock, AVBO}
import arx.graphics.attributeprofiles.SimpleAttributeProfile
import arx.graphics.pov.TopDownCamera
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11
import scalaxy.loops._
import arx.core.vec._

object RawTestbed extends EngineCore {
//  val vbo = new AVBO(SimpleAttributeProfile)
//  var t = 0.0f
//  var pov = new TopDownCamera(20.0f)
//  lazy val shader = ResourceManager.shader("shaders/Simple")
//  lazy val tb = new TextureBlock(128,128)
//  lazy val tc = tb.getOrElseUpdate(ResourceManager.image("default/blank.png"))

  override def update(deltaSeconds: Float): Unit = {
//    t += deltaSeconds
  }

  override def draw(): Unit = {
//    GL.glSetState(GL11.GL_CULL_FACE,enable = false)
//
//    if (vbo.changeState(AVBO.Dirty, AVBO.Updating)) {
//      vbo.clear()
//      val vi = vbo.incrementVertexOffset(4)
//      val ii = vbo.incrementIndexOffset(6)
//      val pb = SimpleAttributeProfile.createPointBuilder()
//
//      for (q <- 0 until 4) {
//        pb.setC(q/4.0f,q/4.0f,q/4.0f,1.0f)
//        pb.setTC(tc(q))
//        val theta = (((2.0f*pi)/4.0f) * q) + t * 0.4
//        pb.setV(cosf(theta) * 10.0f,sinf(theta) * 10.0f,0.0f)
//
//        vbo.setPoint(vi+q,pb)
//      }
//
//      vbo.setIQuad(ii,vi)
//      vbo.changeState(AVBO.Updating, AVBO.Updated)
//    }
//
//    tb.bind()
//    shader.bind()
//    pov.look()
//
//    vbo.solidifyIfNecessary()
//    vbo.drawElements()
//    vbo.changeState(AVBO.Clean, AVBO.Dirty)
  }

  def main(args: Array[String]) {
    scalaMain(args)
  }
}
