

abstract public class MaslClass { 

	public abstract void __update();
	
	// obj->state_n can be translated into:
	// obj._curState = "state_n";
	public String __curState;
	public MaslSimulation __simulation;
	
	public int x;
	public int y;
	public double r;
	public double g;
	public double b;
}
