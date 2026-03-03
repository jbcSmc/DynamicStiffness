import os
os.environ["QT_OPENGL"] = "software"
os.environ["LIBGL_ALWAYS_SOFTWARE"] = "1"
import sys
import numpy as np


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
import matplotlib.pyplot as plt

import pyqtgraph as pg
from pyqtgraph.Qt import QtWidgets

this_dir = os.path.dirname(os.path.abspath(__file__))
build_dir = os.path.abspath(os.path.join(this_dir, "..", "build"))
sys.path.insert(0, build_dir)

import dsm_cpp

class MplCanvas3D(FigureCanvasQTAgg):
    def __init__(self):
        self.fig = Figure()
        self.ax = self.fig.add_subplot(111, projection='3d')
        super().__init__(self.fig)

def read_geometry(filename):
    nodes = {}
    elements = []

    with open(filename, 'r') as f:
        lines = f.readlines()
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        if line.startswith("NODES"):
            n = int(line.split()[1])
            for k in range(n):
                i += 1
                idx, x, y = lines[i].split()
                nodes[int(idx)] = (float(x), float(y))

        elif line.startswith("ELEMENTS"):
            n = int(line.split()[1])
            for k in range(n):
                i += 1
                parts = lines[i].split()
                eid = int(parts[0])
                n1 = int(parts[1])
                n2 = int(parts[2])
                elements.append((eid, n1, n2))

        i += 1

    return nodes, elements
    
def read_geometry3d(filename):
    nodes = {}
    elements = []

    with open(filename, 'r') as f:
        lines = f.readlines()
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        if line.startswith("NODES"):
            n = int(line.split()[1])
            for k in range(n):
                i += 1
                idx, x, y, z = lines[i].split()
                nodes[int(idx)] = (float(x), float(y), float(z))

        elif line.startswith("ELEMENTS"):
            n = int(line.split()[1])
            for k in range(n):
                i += 1
                parts = lines[i].split()
                eid = int(parts[0])
                n1 = int(parts[1])
                n2 = int(parts[2])
                elements.append((eid, n1, n2))

        i += 1

    return nodes, elements

def dsm_solve(self, datafile, exc_node, exc_dof, obs_node, obs_dof, fmin, fmax, npts, fdef):

    U = dsm_cpp.run_dsm(self.datafile, exc_node, exc_dof, obs_node, obs_dof, fmin, fmax, npts, fdef)
    
    f = np.linspace(fmin, fmax, len(U))
    return f, np.array(U)


class MplCanvas(FigureCanvasQTAgg):
    def __init__(self, parent=None):
        self.fig = Figure(figsize=(10,10))
        self.ax = self.fig.add_subplot(111)

        super().__init__(self.fig)

        self.setParent(parent)
        self.setSizePolicy(
            QSizePolicy.Expanding,
            QSizePolicy.Expanding
        )

        self.fig.subplots_adjust(left=0.1, right=0.99, bottom=0.1, top=0.9)

class DSMWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("DSM – Dynamic Stiffness Method")

        self.datafile = None

        self.btn_open = QPushButton("Open data file")
        self.lbl_file = QLabel("No file selected")
        self.btn_view = QPushButton("View")
        self.chk_3d = QCheckBox("3D Structure")
        self.chk_3d.setChecked(False)
        self.chk_nd = QCheckBox("Nodes")
        self.chk_nd.setChecked(False)
        self.chk_el = QCheckBox("Elements")
        self.chk_el.setChecked(False)
        
        self.btn_export = QPushButton("Export view")
        
        self.chk_3d.stateChanged.connect(self.on_toggle_view)

        self.cmb_exc_node = QComboBox()
        self.cmb_exc_dof = QComboBox()
        self.cmb_obs_node = QComboBox()
        self.cmb_obs_dof = QComboBox()



        self.btn_run = QPushButton("Compute")
        
        self.canvas = MplCanvas()
        self.canvas2d = MplCanvas()
        self.canvas3d = MplCanvas3D()
        self.canvas2d.mpl_connect('button_press_event', self.on_click)
        
        self.view_stack = QStackedWidget()
        self.view_stack.addWidget(self.canvas2d)
        self.view_stack.addWidget(self.canvas3d)
        self.view_stack.addWidget(self.canvas)

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
        
        label_fmin = QLabel("f min [Hz]")
        self.fmin_box = QDoubleSpinBox()
        self.fmin_box.setRange(0.0,1e9)
        self.fmin_box.setDecimals(3)
        self.fmin_box.setValue(0.0)
        
        label_fmax = QLabel("f max [Hz]")
        self.fmax_box = QDoubleSpinBox()
        self.fmax_box.setRange(0.0,1e9)
        self.fmax_box.setDecimals(3)
        self.fmax_box.setValue(1000.0)
        
        label_npts = QLabel("Nombre de points de calcul")
        self.npts_box = QSpinBox()
        self.npts_box.setRange(2,1000)
        self.npts_box.setValue(200)
        self.npts_box.setSingleStep(10)
        
        
        label_fdef = QLabel("Displacements at frequency (Hz)")
        self.fdef_box = QDoubleSpinBox()
        self.fdef_box.setRange(0.0,1e9)
        self.fdef_box.setDecimals(3)
        self.fdef_box.setValue(0.0)
        
        
         
        freq_layout.addWidget(label_fmin)
        freq_layout.addWidget(self.fmin_box)
        freq_layout.addWidget(label_fmax)
        freq_layout.addWidget(self.fmax_box)
        freq_layout.addWidget(label_npts)
        freq_layout.addWidget(self.npts_box)
        freq_layout.addWidget(label_fdef)
        freq_layout.addWidget(self.fdef_box)
        

        layout = QVBoxLayout()
        layout.addLayout(top)
        layout.addLayout(controls)
        layout.addLayout(freq_layout)
        layout.addWidget(self.view_stack, stretch=1)       

        container = QWidget()
        container.setLayout(layout)
        self.setCentralWidget(container)

        self.btn_open.clicked.connect(self.open_file)
        self.btn_view.clicked.connect(self.view)
        self.btn_run.clicked.connect(self.exec)
        self.btn_export.clicked.connect(self.export_view_3d)

        self.selected_exc = None   # (node_id, dof)
        self.selected_obs = None
        self.nodes = {}
        self.elements = []
        
    def on_toggle_view(self, state):
        if state == QtCore.Qt.Checked:
            self.view_stack.setCurrentIndex(1)
#           self.plot_structure_3d(self.nodes, self.elements)
        else:
            self.view_stack.setCurrentIndex(0)
#            self.plot_structure(self.nodes, self.elements)
				
    def select_dof(self, node_id, mode):
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
            if self.chk_3d.isChecked():
                self.view_stack.setCurrentIndex(1)
                self.plot_structure_3d(self.nodes, self.elements)
            else:
                self.view_stack.setCurrentIndex(0)
                self.plot_structure(self.nodes, self.elements)
		
    def exec(self):
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
        f, U = dsm_solve(self, self.datafile, node_exc, dof_exc, node_obs, dof_obs, fmin, fmax, npts, fdef)
        
        self.view_stack.setCurrentIndex(2)
        self.canvas.ax.clear()
        self.canvas.ax.set_aspect('auto')
        self.canvas.ax.plot(f, 20*np.log(np.abs(U)), lw=2)
        self.canvas.ax.set_xlabel("Frequency (Hz)")
        self.canvas.ax.set_ylabel("U (dB)")
        
        self.canvas.ax.grid(True)
      
        self.canvas.draw()
       

    def plot_structure(self, nodes, elements):
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
        print("NODES:", len(nodes))
        print("ELEMENTS:", len(elements))
        ax = self.canvas3d.ax
        ax.clear()

        # =====================================================
        # --- ÉLÉMENTS : une seule collection (CRUCIAL)
        # =====================================================
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

        # =====================================================
        # --- NŒUDS : un seul scatter
        # =====================================================
        
        coords = np.array(list(nodes.values()))
        ax.scatter(
            coords[:, 0],
            coords[:, 1],
            coords[:, 2],
            c='b',
            s=5
        )

        # =====================================================
        # --- TEXTES (OPTIONNELS ET COÛTEUX)
        # =====================================================
        if self.chk_el.isChecked():
            for eid, x, y, z in centers:
                ax.text(x, y, z, f"E{eid}", color='purple', fontsize=9)

        if self.chk_nd.isChecked():
            for nid, (x, y, z) in nodes.items():
                ax.text(x, y, z, f"{nid}", color='blue', fontsize=9)

        # =====================================================
        # --- RÉGLAGES GLOBAUX (LÉGERS)
        # =====================================================
        ax.set_xlabel("X")
        ax.set_ylabel("Y")
        ax.set_zlabel("Z")
        ax.set_title("Structure 3D")

        # proportions correctes
        #ax.set_box_aspect([1, 1, 1])
        ax.axis('square')
        # IMPORTANT : éviter axis("equal") en 3D (lent)
        ax.grid(False)

        self.canvas3d.draw_idle()




    
    def find_closest_node(self, x, y, tol=0.05):
        for nid, (xn, yn) in self.nodes.items():
            if (x - xn)**2 + (y - yn)**2 < tol**2:
                return nid
        return None

    def on_click(self, event):
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

if __name__ == "__main__":
   import sys
   from PyQt5.QtWidgets import QApplication
   app = QApplication(sys.argv)
   win = DSMWindow()
   win.show()
   sys.exit(app.exec_())





