// Pybind11 core module (base interface between C++ and Python)
#include <pybind11/pybind11.h>
// Support for automatic conversion between Eigen matrices/vectors and NumPy arrays
#include <pybind11/eigen.h>
#include <pybind11/stl.h>
#include <pybind11/complex.h>

#include "dsm.hpp"

namespace py = pybind11;

// ------------------------------------------------------------
// Python module definition
// ------------------------------------------------------------
// This macro defines the Python module "dsm_cpp" that will be
// imported in Python using:
//
//     import dsm_cpp
//
// "m" is the module object that will contain the exposed
// classes and functions.

PYBIND11_MODULE(dsm_cpp, m)
{
    m.doc() = "DSM solver (Dynamic Stiffness Method)";
    
    // --------------------------------------------------------
    // Expose the C++ class Model to Python
    // --------------------------------------------------------
    // This allows Python to recognize the existence of the
    // Model class defined in C++.
    //
    // Example usage in Python:
    //     m = dsm_cpp.Model()
    //
    // Note: here only the class type is exposed. If you want
    // to access methods or attributes from Python, they must
    // be explicitly added with .def().
    py::class_<Model>(m, "Model");
    
    // --------------------------------------------------------
    // Bind the C++ function read_model()
    // --------------------------------------------------------
    // This function reads the DSM input file and builds the
    // internal representation of the structure.
    //
    // Python usage:
    //     model = dsm_cpp.read_model("structure.dat")
    //
    // py::arg(...) names the parameters so that Python users
    // can call the function using named arguments.
    m.def(
        "read_model",
        &read_model,
        py::arg("filename"),
        "Read DSM input file and build internal model"
    );

    // --------------------------------------------------------
    // Bind the C++ function run_dsm()
    // --------------------------------------------------------
    // This function runs the Dynamic Stiffness Method solver.
    //
    // Parameters:
    //     filename  : DSM input file
    //     exc_node  : excitation node index
    //     exc_dof   : excitation degree of freedom
    //     obs_node  : observation node index
    //     obs_dof   : observation degree of freedom
    //     fmin      : minimum frequency
    //     fmax      : maximum frequency
    //     npts      : number of frequency points
    //     fdef      : deformation frequency
    //
    // The function returns the complex frequency response
    // vector computed by the solver.
    //
    // Python usage example:
    //
    //     U = dsm_cpp.run_dsm(
    //             "model.dat",
    //             exc_node, exc_dof,
    //             obs_node, obs_dof,
    //             fmin, fmax, npts, fdef
    //         )
    //
    // Thanks to pybind11:
    //     - Eigen vectors are converted to NumPy arrays
    //     - std::complex is converted to Python complex
    
    m.def(
        "run_dsm",
        &run_dsm,
        py::arg("filename"),
        py::arg("exc_node"),
        py::arg("exc_dof"),
        py::arg("obs_node"),
        py::arg("obs_dof"),
        py::arg("fmin"),
        py::arg("fmax"),
        py::arg("npts"),
        py::arg("fdef"),
        "Run DSM and return complex response vector"
    );
}


