// Represents a MASL function.
// The generic type denotes the return type of
// the MASL fucntion.
abstract public class MaslFunction<T> {
	
	public abstract T invoke(Object...args);
}
