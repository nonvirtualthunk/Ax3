package arx.tyche.graphics.data

import arx.engine.entity.{GameEntity, TGameEntity}
import arx.engine.graphics.data.TGraphicsData

class SelectionData extends TGraphicsData{
	var selectedSpirit : TGameEntity = GameEntity.Sentinel
}
