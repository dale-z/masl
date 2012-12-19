import java.awt.Color;
import java.awt.Graphics;
import javax.swing.JFrame;

// A MASL program is called a simulation. It will be translated into
// a subclass of this Java class MaslSimulation.
public abstract class MaslSimulation extends JFrame {

	// The top level code of a MASL program goes into this method.
	// MASL class definition stays outside this methods,
	// as inner classes of a MaslSimulation subclass.
	public abstract void init();
	
	// This list contains all the agents to be updated in every time step 
	// Use the name "container" to access it. 
//	ArrayList<MaslClass> __container;
	
	public void paint(Graphics g) {

		g.clearRect(0, 0, nx*cellSize, ny*cellSize);
		
		// Draw all the agents.
		for(int i = 0; i < __list.size(); ++i) {
			
			MaslClass agent = __list.get(i);
			g.setColor(new Color((float)agent.r, (float)agent.g, (float)agent.b));
			int x = agent.x;
			int y = agent.y;
			g.fillRect(x * cellSize, y * cellSize, cellSize, cellSize);
			g.setColor(new Color(0.0f, 0.0f, 0.0f));
			g.drawRect(x * cellSize, y * cellSize, cellSize, cellSize);
		}
	}

	public void _run(MaslList<MaslClass> list) {

		__list = list;
		
		// Set up the drawing area.
		setTitle("MASL Simulation");
		setSize(cellSize * nx, cellSize * ny);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
        setLocationRelativeTo(null);
        setVisible(true);
        setResizable(false);
		
        // Here we go...
		while(true) {
			
			// Repaint the drawing area to reflect the updated agents.
			repaint();
			
			// Wait for a while before the next step.
			try {
				Thread.sleep(interval);
			} catch (InterruptedException e) {
			}

			// Update all the agents.
			for(int i = 0; i < list.size(); ++i) {				
				list.get(i).__update();
			}
		}
	}

	// Wrapper for run function
	public MaslFunction<Void> run = new MaslFunction<Void>() {
		public Void invoke(Object... args) {

			// The first few lines of the overridden 'invoke' method convert
			// the arguments into types specified by the MASL function.
			MaslList<MaslClass> list = (MaslList<MaslClass>) args[0];
			_run(list);
			return null;
		}	
	};

	// Print a char
	public MaslFunction<Void> printChar = new MaslFunction<Void>(){
		public Void invoke(Object... args) {
			Character o = (Character) args[0];
			System.out.print(o);
			return null;
		}	
	};
	// Print a int
	public MaslFunction<Void> printInt = new MaslFunction<Void>(){
		public Void invoke(Object... args) {
			Integer o = (Integer) args[0];
			System.out.print(o);
			return null;
		}	
	};
	// Print a double
	public MaslFunction<Void> printDouble = new MaslFunction<Void>(){
		public Void invoke(Object... args) {
			Double o = (Double) args[0];
			System.out.print(o);
			return null;
		}	
	};
	// Print a boolean
	public MaslFunction<Void> printBool = new MaslFunction<Void>(){
		public Void invoke(Object... args) {
			Boolean o = (Boolean) args[0];
			System.out.print(o);
			return null;
		}	
	};
	// Print a MaslObject
	/*public MaslFunction<Void> printObj = new MaslFunction<Void> {
		public Void invoke(Object... args) {
			MaslObject o = (MaslObject) args[0];
			System.out.print(o);
		}	
	};*/
	// Print a MaslList
	public MaslFunction<Void> printList = new MaslFunction<Void>(){
		public Void invoke(Object... args) {
			MaslList<? extends Object> list = (MaslList<? extends Object>) args[0];
			String toPrint = "[";
			for(Object o : list) {
				toPrint += o;
				toPrint += ";";
			}
			toPrint += "]";
			System.out.print(toPrint);
			return null;
		}	
	};
	// Print a String
	public MaslFunction<Void> printStr = new MaslFunction<Void>(){
		public Void invoke(Object... args) {
			MaslList<? extends Object> list = (MaslList<? extends Object>) args[0];
			String toPrint = "";
			for(Object o : list) {
				toPrint += o;
			}
			System.out.print(toPrint);
			return null;
		}	
	};
	
	// The number of cells in a row.
	public int nx = 10;
	// The number of cells in a column.
	public int ny = 10;
	// The size of a cell.
	public int cellSize = 30;
	// The number of milliseconds to be paused between two steps. 
	public int interval = 2000;
	
	private MaslList<MaslClass> __list;
}
