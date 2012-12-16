/*
	MASL Code:

	int sum(int a, int b) {

		return a + b;
	}
 */

// This class is a sample translation for the MASL snippet above.
// The generic type is now replaced with the return type of the MASL function.
public class MaslFunctionSampleFunction extends MaslFunction<Integer> {

	@Override
	public Integer invoke(Object... args) {

		// The first few lines of the overridden 'invoke' method convert
		// the arguments into types specified by the MASL function.
		
		int a = (Integer) args[0];
		int b = (Integer) args[1];
		
		return a + b;
	}	
}
