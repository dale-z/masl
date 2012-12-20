import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;

// Represents a MASL list.
class MaslList<T> implements Iterable<T> {

	public MaslList(T...ts) {

		_list = new ArrayList<T>(Arrays.asList(ts));
	}

	private MaslList(ArrayList<T> list) {
		_list = list;
	}

	public T get(int i) {
		return _list.get(i);
	}

	public void set(int i, T t) {
		_list.set(i, t);
	}

	public void insert(int i, T t) {
		_list.add(i, t);
	}

	public T remove(int i) {
		return _list.remove(i);
	}

	public void append(T t) {
		_list.add(t);
	}

	public MaslList<T> filter(MaslFunction<Boolean> f) {

		ArrayList<T> newList = new ArrayList<T>();
		for(T e : _list)
			if(f.invoke(e)) newList.add(e);

		return new MaslList<T>(newList);
	}

	public int count(MaslFunction<Boolean> f) {

		int n = 0;

		for(T e : _list)
			if(f.invoke(e)) ++n;

		return n;
	}

	public int size() {
		return _list.size();
	}

	public String toString() {
		return _list.toString();
	}

	private ArrayList<T> _list;

	@Override
	public Iterator<T> iterator() {
		// TODO Auto-generated method stub
		return _list.iterator();
	}
}
