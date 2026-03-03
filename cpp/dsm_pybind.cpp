#include <pybind11/pybind11.h>
#include <pybind11/eigen.h>
#include <pybind11/stl.h>
#include <pybind11/complex.h>

#include "dsm.hpp"

namespace py = pybind11;


PYBIND11_MODULE(dsm_cpp, m)
{
    m.doc() = "DSM solver (Dynamic Stiffness Method)";
    py::class_<Model>(m, "Model");

    m.def(
        "read_model",
        &read_model,
        py::arg("filename"),
        "Read DSM input file and build internal model"
    );

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


