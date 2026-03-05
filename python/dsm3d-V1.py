"""
main.py - DSM PyQt5 GUI for Dynamic Stiffness Method
Version: 2.0
Author: Jean-Baptiste CASIMIR - ISAE-Supméca
License: GNU General Public License v3 (GPLv3)

Copyright (C) 2026 Jean-Baptiste CASIMIR

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
"""

# =====================================================
# === Standard Library Imports
# =====================================================
import os
import sys
import numpy as np

os.environ["QT_OPENGL"] = "software"
os.environ["LIBGL_ALWAYS_SOFTWARE"] = "1"

# =====================================================
# === Qt and Visualization Libraries
# =====================================================
from PyQt5 import QtCore
from PyQt5.QtWidgets import (
    QApplication, QMainWindow, QWidget,
    QFileDialog, QPushButton, QLabel, QCheckBox,
    QVBoxLayout, QHBoxLayout, QComboBox,
    QInputDialog, QDoubleSpinBox, QMessageBox,
    QSpinBox, QSizePolicy, QStackedWidget
)

from mpl_toolkits.mplot3d.art3d import Line3DCollection
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg
from matplotlib.figure import Figure

# =====================================================
# === Import C++ DSM Solver Module
# =====================================================
this_dir = os.path.dirname(os.path.abspath(__file__))
build_dir = os.path.abspath(os.path.join(this_dir, "..", "build"))
sys.path.insert(0, build_dir)
import dsm_cpp

# =====================================================
# === Matplotlib Canvas Classes
# =====================================================
class MplCanvas3D(FigureCanvasQTAgg):
    """
    3D Matplotlib canvas for structural visualization.
    """
    def __init__(self):
        self.fig = Figure()
        self.ax = self.fig.add_subplot(111, projection='3d')
        super().__init__(self.fig)

class MplCanvas(FigureCanvasQTAgg):
    """
    2D Matplotlib canvas for structural plots and frequency response.
    """
    def __init__(self, parent=None):
        self.fig = Figure(figsize=(10, 10))
        self.ax = self.fig.add_subplot(111)
        super().__init__(self.fig)

        self.setParent(parent)
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)

        # Adjust margins for better readability
        self.fig.subplots_adjust(left=0.1, right=0.99, bottom=0.1, top=0.9)
        
# =====================================================
# === Geometry Readers
# =====================================================
def read_geometry(filename):
    """
    Read 2D frame geometry from file.

    Returns
    -------
    nodes : dict
        node_id -> (x, y)
    elements : list
        list of (element_id, node1, node2)
    """
    nodes = {}
    elements = []

    with open(filename, 'r') as f:
        lines = f.readlines()

    i = 0
    while i < len(lines):
        line = lines[i].strip()

        if line.startswith("NODES"):
            n = int(line.split()[1])
            for _ in range(n):
                i += 1
                idx, x, y = lines[i].split()
                nodes[int(idx)] = (float(x), float(y))

        elif line.startswith("ELEMENTS"):
            n = int(line.split()[1])
            for _ in range(n):
                i += 1
                parts = lines[i].split()
                elements.append((int(parts[0]), int(parts[1]), int(parts[2])))

        i += 1

    return nodes, elements


def read_geometry3d(filename):
    """
    Read 3D frame geometry from file.

    Returns
    -------
    nodes : dict
        node_id -> (x, y, z)
    elements : list
        list of (element_id, node1, node2)
    """
    nodes = {}
    elements = []

    with open(filename, 'r') as f:
        lines = f.readlines()

    i = 0
    while i < len(lines):
        line = lines[i].strip()

        if line.startswith("NODES"):
            n = int(line.split()[1])
            for _ in range(n):
                i += 1
                idx, x, y, z = lines[i].split()
                nodes[int(idx)] = (float(x), float(y), float(z))

        elif line.startswith("ELEMENTS"):
            n = int(line.split()[1])
            for _ in range(n):
                i += 1
                parts = lines[i].split()
                elements.append((int(parts[0]), int(parts[1]), int(parts[2])))

        i += 1

    return nodes, elements

# =====================================================
# === DSM Solver Interface
# =====================================================
def dsm_solve(datafile, exc_node, exc_dof,
              obs_node, obs_dof,
              fmin, fmax, npts, fdef):
    """
    Execute DSM computation via the C++ backend.

    Returns
    -------
    f : ndarray
        Frequency vector
    U : ndarray
        Complex displacement response
    """
    U = dsm_cpp.run_dsm(
        datafile,
        exc_node, exc_dof,
        obs_node, obs_dof,
        fmin, fmax, npts, fdef
    )

    f = np.linspace(fmin, fmax, len(U))
    return f, np.array(U)



# =====================================================
# === Main GUI Window
# =====================================================
class DSMWindow(QMainWindow):
    """
    Main GUI for the Dynamic Stiffness Method application.

    Responsibilities:
    - Load structural model files
    - Visualize 2D and 3D structures
    - Configure excitation/observation DOFs
    - Execute DSM solver
    - Display frequency response
    """

    def __init__(self):
        super().__init__()

        self.setWindowTitle("DSM – Dynamic Stiffness Method")

        # Currently loaded data file
        self.datafile = None

        # Storage for geometry
        self.nodes = {}
        self.elements = []

        # Selected excitation and observation
        self.selected_exc = None
        self.selected_obs = None

        # ================= UI Controls =================
        # Buttons (open file, view, compute, export view)
        self.btn_open = QPushButton("Open data file")
        self.btn_view = QPushButton("View")
        self.btn_run = QPushButton("Compute")
        self.btn_export = QPushButton("Export view")

        self.lbl_file = QLabel("No file selected")

        # Check boxes (3D structure, Nodes, Elemnts)
        self.chk_3d = QCheckBox("3D Structure")
        self.chk_nd = QCheckBox("Nodes")
        self.chk_el = QCheckBox("Elements")
        self.chk_3d.stateChanged.connect(self.on_toggle_view)

        # Combo boxes for input/output selection
        self.cmb_exc_node = QComboBox()
        self.cmb_exc_dof = QComboBox()
        self.cmb_obs_node = QComboBox()
        self.cmb_obs_dof = QComboBox()

        # Frequency controls
        self.fmin_box = QDoubleSpinBox()
        self.fmax_box = QDoubleSpinBox()
        self.npts_box = QSpinBox()
        self.fdef_box = QDoubleSpinBox()
        self._configure_frequency_controls()

        # ================= Canvases =================
        self.canvas2d = MplCanvas()
        self.canvas3d = MplCanvas3D()
        self.canvas = MplCanvas()  # response plot
        self.canvas2d.mpl_connect('button_press_event', self.on_click)

        # Stack view: 2D / 3D / Frequency response
        self.view_stack = QStackedWidget()
        self.view_stack.addWidget(self.canvas2d)
        self.view_stack.addWidget(self.canvas3d)
        self.view_stack.addWidget(self.canvas)

        self._build_layout()
        self._connect_signals()

    # =====================================================
    # === Internal UI Configuration
    # =====================================================
    def _configure_frequency_controls(self):
        """Configure frequency input widgets."""
        self.fmin_box.setRange(0.0, 1e9)
        self.fmax_box.setRange(0.0, 1e9)
        self.fdef_box.setRange(0.0, 1e9)
        self.npts_box.setRange(2, 1000)
        
        self.fmax_box.setValue(1000.0)
        self.npts_box.setValue(200)

    def _build_layout(self):
        """Create main window layout."""
        top = QHBoxLayout()
        top.addWidget(self.btn_open)
        top.addWidget(self.lbl_file)
        top.addWidget(self.btn_view)
        top.addWidget(self.chk_3d)
        top.addWidget(self.chk_nd)
        top.addWidget(self.chk_el)
        top.addWidget(self.btn_export)

        controls = QHBoxLayout()
        controls.addWidget(QLabel("Input"))
        controls.addWidget(self.cmb_exc_node)
        controls.addWidget(self.cmb_exc_dof)
        controls.addWidget(QLabel("Output"))
        controls.addWidget(self.cmb_obs_node)
        controls.addWidget(self.cmb_obs_dof)
        controls.addWidget(self.btn_run)

        freq_layout = QHBoxLayout()
        freq_layout.addWidget(QLabel("f min [Hz]"))
        freq_layout.addWidget(self.fmin_box)
        freq_layout.addWidget(QLabel("f max [Hz]"))
        freq_layout.addWidget(self.fmax_box)
        freq_layout.addWidget(QLabel("Number of points"))
        freq_layout.addWidget(self.npts_box)
        freq_layout.addWidget(QLabel("Deformation frequency [Hz]"))
        freq_layout.addWidget(self.fdef_box)

        layout = QVBoxLayout()
        layout.addLayout(top)
        layout.addLayout(controls)
        layout.addLayout(freq_layout)
        layout.addWidget(self.view_stack)

        container = QWidget()
        container.setLayout(layout)
        self.setCentralWidget(container)

    def _connect_signals(self):
        """Connect buttons to callbacks."""
        self.btn_open.clicked.connect(self.open_file)
        self.btn_view.clicked.connect(self.view)
        self.btn_run.clicked.connect(self.exec)
        self.btn_export.clicked.connect(self.export_view_3d)
 
        
    def on_toggle_view(self, state):
        """
        Switch between 2D and 3D structure visualization.

        This function is triggered when the '3D Structure' checkbox
        changes state. The GUI uses a QStackedWidget (view_stack)
        containing multiple canvases:

        index 0 : 2D structure view
        index 1 : 3D structure view
        index 2 : frequency response plot

        Parameters
        ----------
        state : int
            Checkbox state returned by Qt (Checked / Unchecked).
        """
        
        if state == QtCore.Qt.Checked:
            self.view_stack.setCurrentIndex(1)
        else:
            self.view_stack.setCurrentIndex(0)
				
				
    def select_dof(self, node_id, mode):
        """
        Select a degree of freedom (DOF) for excitation or observation.

        When the user clicks on a node in the structure plot,
        a dialog appears allowing the user to choose the DOF.

        Parameters
        ----------
        node_id : int
            ID of the selected node.
        mode : str
            Selection mode:
                "exc" → excitation DOF
                "obs" → observation DOF
        """
        
        items = ["U\u2093", "U\u1D67", "\u03B8\u2093"]
        dof, ok = QInputDialog.getItem(
            self,
            "Select DOF",
            f"Node {node_id} – choose DOF:",
            items,
            0,
            False
        )
        if not ok:
            return

        if mode == "exc":
            self.selected_exc = (node_id, dof)
            index = self.cmb_exc_node.findText(f"Node {self.selected_exc[0]}")
            self.cmb_exc_node.setCurrentIndex(index)
            if self.selected_exc[1]=='U\u2093':
                index = 0
            elif self.selected_exc[1]=='U\u1D67':
                index = 1
            else:
                index = 2
            self.cmb_exc_dof.setCurrentIndex(index)
            
        else:
            self.selected_obs = (node_id, dof)
            index = self.cmb_obs_node.findText(f"Node {self.selected_obs[0]}")
            self.cmb_obs_node.setCurrentIndex(index)
            if self.selected_obs[1]=='U\u2093':
                index = 0
            elif self.selected_obs[1]=='U\u1D67':
                index = 1
            else:
                index = 2
            self.cmb_obs_dof.setCurrentIndex(index)
            
        self.plot_structure(self.nodes, self.elements)

    def open_file(self):
        """
        Open a DSM input data file and load the structure geometry.

        The file must start with a keyword indicating the problem type:
            - '2DFRAME'
            - '3DFRAME'

        The geometry (nodes and elements) is then read and displayed
        in the appropriate visualization canvas.
        """
        
        fname, _ = QFileDialog.getOpenFileName(
            self, "Open DSM data file", "", "Data files (*.dat *.txt)"
        )
        
        if fname:
            with open(fname, "r") as f:
                problem_type = f.read().split()[0]

            if problem_type == "2DFRAME":
                print("Structure 2D")
            elif problem_type == "3DFRAME":
                print("Structure 3D")
            else:
                raise ValueError(f"Unknown problem type: {problem_type}")
            
        
            self.datafile = fname
            self.lbl_file.setText(fname)
            if problem_type == "3DFRAME" :
                self.chk_3d.setChecked(True)
                self.nodes, self.elements = read_geometry3d(fname)
                self.plot_structure_3d(self.nodes, self.elements)
            elif problem_type == "2DFRAME" :
                self.chk_3d.setChecked(False)
                self.nodes, self.elements = read_geometry(fname)
                self.plot_structure(self.nodes, self.elements)
            
            self.cmb_exc_node.clear()
            self.cmb_obs_node.clear()
            for i in range(len(self.nodes)):
                self.cmb_exc_node.addItem(f"Node {i+1}")
                self.cmb_obs_node.addItem(f"Node {i+1}")
            
            self.cmb_exc_dof.clear()
            self.cmb_obs_dof.clear()
            
            self.cmb_exc_dof.addItem("Ux")
            self.cmb_exc_dof.addItem("Uy")  
            if self.chk_3d.isChecked():    
                self.cmb_exc_dof.addItem("Uz")   
                self.cmb_exc_dof.addItem("Rx") 
                self.cmb_exc_dof.addItem("Ry")          
            self.cmb_exc_dof.addItem("Rz")

            self.cmb_obs_dof.addItem("Ux")
            self.cmb_obs_dof.addItem("Uy")              
            if self.chk_3d.isChecked():
                self.cmb_obs_dof.addItem("Uz")
                self.cmb_obs_dof.addItem("Rx") 
                self.cmb_obs_dof.addItem("Ry")
            self.cmb_obs_dof.addItem("Rz")
            
    
    def view(self):
       """
       Display the structure either in 2D or 3D depending on the checkbox state.
       """
       if self.chk_3d.isChecked():
          self.view_stack.setCurrentIndex(1)
          self.plot_structure_3d(self.nodes, self.elements)
       else:
          self.view_stack.setCurrentIndex(0)
          self.plot_structure(self.nodes, self.elements)
		
    def exec(self):
        """
        Run the DSM computation using the selected parameters.

        Steps:
           1. Read excitation/observation DOFs
           2. Validate frequency range
           3. Call C++ DSM backend
           4. Plot frequency response in dB
        """
        if not self.datafile:
            return

        node_exc = self.cmb_exc_node.currentIndex()
        dof_exc = self.cmb_exc_dof.currentIndex()
        
        node_obs = self.cmb_obs_node.currentIndex()
        dof_obs = self.cmb_obs_dof.currentIndex()
        
        fmin = self.fmin_box.value()
        fmax = self.fmax_box.value()
        npts = self.npts_box.value()
        fdef = self.fdef_box.value()
        
        if fmax <= fmin:
            QMessageBox.warning(
            self,
            "Invalid frequencies",
            "f max must be strictly greater than f min"
            )
            return
        
        if not fmin <= fdef <= fmax:
            QMessageBox.warning(
            self,
            "Invalid displacement frequency",
            "Displacement frequencty must be between f min and f max"
            )
            return		
            	
        f, U = dsm_solve(self.datafile, node_exc, dof_exc, node_obs, dof_obs, fmin, fmax, npts, fdef)
        
        self.view_stack.setCurrentIndex(2)
        self.canvas.ax.clear()
        self.canvas.ax.set_aspect('auto')
        self.canvas.ax.plot(f, 20*np.log(np.abs(U)), lw=2)
        self.canvas.ax.set_xlabel("Frequency (Hz)")
        self.canvas.ax.set_ylabel("U (dB)")
        
        self.canvas.ax.grid(True)
      
        self.canvas.draw()
       

    def plot_structure(self, nodes, elements):
        """
        Plot a 2D frame structure.

        Parameters
        ----------
        nodes : dict
            Dictionary mapping node_id -> (x, y)
        elements : list
            List of tuples (element_id, node1, node2)

        Displays:
            - Elements as black lines
            - Local axis direction (red arrow)
            - Node numbers
            - Element numbers (optional)
            - Excitation and observation markers
        """
        ax = self.canvas2d.ax
        ax.clear()

        # --- éléments ---
        for eid, n1, n2 in elements:
            x1, y1 = nodes[n1]
            x2, y2 = nodes[n2]

            # ligne élément
            ax.plot([x1, x2], [y1, y2], 'k-', lw=2)

            # milieu
            xm = 0.5 * (x1 + x2)
            ym = 0.5 * (y1 + y2)
             # direction locale
            dx = x2 - x1
            dy = y2 - y1
            L = (dx**2 + dy**2)**0.5
            tx = dx / L
            ty = dy / L

            # longueur flèche (20 % de l'élément)
            s = 0.2 * L

            ax.arrow(
                xm, ym, s * tx, s * ty,
                head_width=0.05 * L,
                head_length=0.08 * L,
                fc='red', ec='red',
                length_includes_head=True
            )

            # numéro d’élément
            ax.text(
                xm, ym,
                f"E{eid}",
                color='purple',
                fontsize=9,
                ha='center', va='bottom'
            )

        # --- nœuds ---
        for nid, (x, y) in nodes.items():
            ax.plot(x, y, 'bo')
            ax.text(x, y, f"{nid}", color='blue', fontsize=10,
                    ha='right', va='bottom')

        ax.set_aspect('equal')
        ax.set_xlabel("X")
        ax.set_ylabel("Y")
        ax.set_title("Structure – elements and local axes")
        ax.grid(True)

        self.canvas2d.draw()
        
        # --- excitation ---
        if self.selected_exc:
            nid, dof = self.selected_exc
            x, y = nodes[nid]
            ax.plot(x, y, 'ro', markersize=12)
            ax.text(x, y, f"EXC {dof}", color='red',
                    ha='left', va='top')

            # --- observation ---
        if self.selected_obs:
            nid, dof = self.selected_obs
            x, y = nodes[nid]
            ax.plot(x, y, 'go', markersize=12)
            ax.text(x, y, f"OBS {dof}", color='green',
                    ha='left', va='bottom')



    def plot_structure_3d(self, nodes, elements):
        """
        Plot a 2D frame structure.

        Parameters
        ----------
        nodes : dict
            Dictionary mapping node_id -> (x, y)
        elements : list
            List of tuples (element_id, node1, node2)

        Displays:
            - Elements as black lines
            - Local axis direction (red arrow)
            - Node numbers
            - Element numbers (optional)
            - Excitation and observation markers
        """
        
        print("NODES:", len(nodes))
        print("ELEMENTS:", len(elements))
        ax = self.canvas3d.ax
        ax.clear()

        segments = []
        centers = []

        for eid, n1, n2 in elements:
            x1, y1, z1 = nodes[n1]
            x2, y2, z2 = nodes[n2]

            segments.append([
                (x1, y1, z1),
                (x2, y2, z2)
            ])

            centers.append((eid,
                            0.5 * (x1 + x2),
                            0.5 * (y1 + y2),
                            0.5 * (z1 + z2)))

        line_collection = Line3DCollection(
            segments,
            colors='k',
            linewidths=2
        )
        ax.add_collection3d(line_collection)
        
        coords = np.array(list(nodes.values()))
        ax.scatter(
            coords[:, 0],
            coords[:, 1],
            coords[:, 2],
            c='b',
            s=5
        )

        if self.chk_el.isChecked():
            for eid, x, y, z in centers:
                ax.text(x, y, z, f"E{eid}", color='purple', fontsize=9)

        if self.chk_nd.isChecked():
            for nid, (x, y, z) in nodes.items():
                ax.text(x, y, z, f"{nid}", color='blue', fontsize=9)

        ax.set_xlabel("X")
        ax.set_ylabel("Y")
        ax.set_zlabel("Z")
        ax.set_title("Structure 3D")

        ax.axis('square')
        ax.grid(False)

        self.canvas3d.draw_idle()




    
    def find_closest_node(self, x, y, tol=0.05):
        """
        Find the node whose coordinates are closest to a given point.

        Parameters
        ----------
        x, y : float
            Coordinates of the point (typically a mouse click in the plot).

        tol : float
            Distance tolerance used to detect a node.

        Returns
        -------
        nid : int or None
            Identifier of the node if a node is found within the tolerance,
            otherwise None.
        """
        
        for nid, (xn, yn) in self.nodes.items():
            if (x - xn)**2 + (y - yn)**2 < tol**2:
                return nid
        return None


    def on_click(self, event):
        """
        Handle mouse click on 2D canvas.

        Left click:
            Select excitation node
            Shift + Left click:
            Select observation node
        """
        modifiers = QApplication.keyboardModifiers()
        if event.inaxes != self.canvas.ax:
            return
        if not self.nodes:
            return

        nid = self.find_closest_node(event.xdata, event.ydata)
        if nid is None:
            return

        if event.button == 1:
            if modifiers & Qt.ShiftModifier:
                self.select_dof(nid, mode="obs")
            else:
                self.select_dof(nid, mode="exc")

    def export_view_3d(self):
        filename, _ = QFileDialog.getSaveFileName(
            self,
            "Export 3D view",
            "",
            "PNG (*.png);;PDF (*.pdf)"
        )
        if filename:
            self.canvas3d.fig.savefig(filename, dpi=300, bbox_inches="tight")

# =====================================================
# === Main entry point ===
# =====================================================
if __name__ == "__main__":
   import sys
   from PyQt5.QtWidgets import QApplication
   app = QApplication(sys.argv)
   win = DSMWindow()
   win.show()
   sys.exit(app.exec_())





