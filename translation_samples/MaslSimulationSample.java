
public class MaslSimulationSample extends MaslSimulation {

	@Override
	public void init() {
		
		// Translated from MASL.
	}

	
	public static void main(String[] args) {
		
		// Each simulation is executable since it has a main()
		// method. It will initialize itself (a window indeed)
		// and start the simulation.
		MaslSimulationSample sample = new MaslSimulationSample();
		sample.init();
	}
}
