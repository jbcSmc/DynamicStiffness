#pragma once

#include <vector>
#include <complex>
#include <string>
#include <Eigen/Dense>

using cdouble = std::complex<double>;
using MatrixXc = Eigen::Matrix<cdouble, Eigen::Dynamic, Eigen::Dynamic>;
using VectorXc = Eigen::Matrix<cdouble, Eigen::Dynamic, 1>;
/// =====================
/// Données de base
/// =====================
struct Frequencies {
   double fmin;
   double fmax;
   int n;
};

struct Load {
    int node_id;
    int dof;
    std::complex<double> value;
};
struct Node {
    int id;         // identifiant (1-based comme Fortran)
    double x, y, z;    // coordonnées
};

struct Element {
    int id;
    int node1, node2;     // nœuds extrémités (ids)
    int material_id;     // matériau
    int section_id;     // section
    double V[3];
};

struct Material {
    int id;
    double rho;
    double E;
    double G;
    double eta;     // amortissement (si présent)
    double nu;
};

struct Section {
    int id;
    double A;
    double Iy;
    double Iz;
    double J;
    double kappay;
    double kappaz;
};

/// =====================
/// Modèle global DSM
/// =====================

struct Model {
    std::string problem_type;
    std::vector<Node>     nodes;
    std::vector<Element> elements;
    std::vector<Material> materials;
    std::vector<Section>  sections;
    std::vector<Load> loads;
    std::vector<int> fdof;
    Frequencies frequencies;
};


Model read_model(const std::string& filename);

// 05/03 std::vector<std::complex<double>> run_dsm(const std::string& filename,
VectorXc run_dsm(const std::string& filename,
        int exc_node, int exc_dof,
        int obs_node, int obs_dof,
        double fmin, double fmax, int npts, double fdef);

