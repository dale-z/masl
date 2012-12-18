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
		
		// Draw all the agents.
		for(int i = 0; i < __list.size(); ++i) {
			
			MaslClass agent = __list.get(i);
			g.setColor(new Color(agent.r, agent.g, agent.b));
			int x = agent.x;
			int y = agent.y;
			g.fillRect(x * cellSize, y * cellSize, cellSize, cellSize);
		}
	}
	
	public void run(MaslList<MaslClass> list) {

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
			
			// Update all the agents.
			for(int i = 0; i < list.size(); ++i) {				
				list.get(i).__update();
			}
			
			// Repaint the drawing area to reflect the updated agents.
			repaint();
			
			// Wait for a while before the next step.
			try {
				Thread.sleep(interval);
			} catch (InterruptedException e) {
			}
		}
	}
	
	// The number of cells in a row.
	public int nx = 10;
	// The number of cells in a column.
	public int ny = 10;
	// The size of a cell.
	public int cellSize = 30;
	// The number of milliseconds to be paused between two steps. 
	public int interval = 30;
	
	private MaslList<MaslClass> __list;
}
