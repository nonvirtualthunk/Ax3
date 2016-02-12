package arx.axistential.graphics.components.environmentviewer.viewlayers

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/14
 * Time: 5:13 PM
 */

import arx.axistential.graphics.components.environmentviewer.TEnvironmentViewerLayer
import arx.graphics.shader.TShader

trait TUIViewLayer extends TEnvironmentViewerLayer {
	def shader : TShader
}
