package arx.axistential.graphics.components.general

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/19/12
 * Time: 2:35 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.core.Moddable
import arx.core.units.UnitOfTime

class VoxelAnimation ( var timeout : UnitOfTime ) {
	var age : UnitOfTime = 0.seconds
	var voxels : Moddable[List[AnimationSubVoxel]] = Nil

	var finished = false
}
