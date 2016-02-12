package arx.axistential.game.logic.ai.goals;

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/5/15
 * Time: 3:51 PM
 */
public enum MoveGait {
    Stationary(0.0f),
    Saunter(0.25f),
    Sneak(0.5f),
    SlowWalk(0.5f),
    Walk(1.0f),
    Jog(1.5f),
    Run(2.0f),
    Sprint(2.5f);

    public float speedMultiplier;

    MoveGait(float speedMultiplier) {
        this.speedMultiplier = speedMultiplier;
    }
}
