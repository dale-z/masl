/*

	class MaslClassSampleClass {
	
		state state1 {
			// Do something...
		}
		
		state state2 {
			// Do something...
		}
		
		state state3 {
			// Do something...
		}
		
		int x = 3;
		double y;
		
		double sum(double a, double b) {
			return a + b;
		}
	}

 */

// This class is a sample translation for the MASL snippet above.
public class MaslClassSampleClass extends MaslClass {

	public void __update() {
		
		switch(__curState) {
			case "state1":
				state1();
				break;
			case "state2":
				state2();
				break;
			case "state3":
				state3();
				break;
			default:
				// If the current state is set to a non-existent one, do nothing.
				break;
		}
	}

	private void state1() {
		// Do something.
	}
	
	private void state2() {
		// Do something.
	}
	
	private void state3() {
		// Do something.
	}
	
	public int x = 3;
	public double y = 0;
	public MaslFunction<Double> a = new MaslFunction<Double>() {

		@Override
		public Double invoke(Object... args) {

			Double a = (Double) args[0];
			Double b = (Double) args[1];
			
			return a + b;
		}
	};
}
