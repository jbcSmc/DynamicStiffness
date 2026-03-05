#include <iostream>
#include <fstream>
#include <stdexcept>
#include <complex>
#include <vector>

#include <Eigen/Dense>
#include <cmath>
#include "dsm.hpp"



using namespace std;

inline int dof(int node_id, int local_dof){
    // node_id commence à 1 (comme dans le fichier)
    // local_dof : 0=ux, 1=uy, 2=theta
    return 3 * (node_id - 1) + local_dof;
}

inline int dof3D(int node_id, int local_dof){
    // node_id commence à 1 (comme dans le fichier)
    // local_dof : 0=ux, 1=uy, 2=theta
    return 6 * (node_id - 1) + local_dof;
}

Eigen::IOFormat fmtMat(
    Eigen::StreamPrecision,
    Eigen::DontAlignCols,
    "\t\t",
    "\n",
    "",
    "",
    "",
    ""
    );
    
    Eigen::IOFormat fmtVec(
    Eigen::StreamPrecision,
    Eigen::DontAlignCols,
    "\t\t",
    "\n",
    "",
    "",
    "",
    ""
    );
    
MatrixXc Traction(double w, double S, double L, double rho, cdouble EC){
	MatrixXc KW = MatrixXc::Zero(2,2);
    cdouble k = w * sqrt(rho/EC);
	
	KW(0,1)=-EC * S * k / sin(k * L);
    KW(1,0)=KW(0,1);
    KW(0,0)=-KW(0,1)*cos( k * L);
    KW(1,1)=KW(0,0);
	
	return KW;
}

MatrixXc Torsion(double w, double S, double J, double IO, double L, double rho, cdouble GC){
	MatrixXc KW = MatrixXc::Zero(2,2);
	
    cdouble k = w * sqrt(rho * IO / GC / J), fact;
    fact = w * sqrt( rho * GC * J * IO);
	
	KW(0,0)= fact * cos(k * L) / sin(k * L);
	KW(0,1) = -fact / sin(k * L);
    KW(1,0)=KW(0,1);
    KW(1,1)=KW(0,0);
    
	return KW;
}

MatrixXc XYBending(double w, double S, double Iz, double L, double rho,
                   cdouble EC){
    MatrixXc KW = MatrixXc::Zero(4,4);
    cdouble k=pow(w*w*rho*S/EC/Iz,0.25), fact;
    fact = k * EC * Iz / (1.0 - cos(k * L) * cosh(k * L));
    
    KW(0,0)=fact * k * k * (cos(k * L)*sinh(k * L) + sin( k * L) * cosh (k * L));
    KW(0,1)=fact * k * sin(k * L) * sinh(k * L);
    KW(0,2)=fact * k * k * (-sin(k * L) - sinh(k * L));
    KW(0,3)=fact * k * (cosh(k * L) - cos(k * L));
    KW(1,0)=KW(0,1);
    KW(1,1)=fact * (sin(k * L) * cosh(k * L) - cos(k * L) * sinh(k * L));
    KW(1,2)=-KW(0,3);
    KW(1,3)=fact * (sinh(k * L) - sin(k * L));
    KW(2,0)=KW(0,2);
    KW(2,1)=KW(1,2);
    KW(2,2)=KW(0,0);
    KW(2,3)=-KW(0,1);
    KW(3,0)=KW(0,3);
    KW(3,1)=KW(1,3);
    KW(3,2)=KW(2,3);
    KW(3,3)=KW(1,1); 
    return KW;					      
}

MatrixXc XZBending(double w, double S, double Iy, double L, double rho,
                   cdouble EC){
    MatrixXc KW = MatrixXc::Zero(4,4);
    
    cdouble k=pow(w*w*rho*S/EC/Iy,0.25), fact;
    fact = k * EC * Iy / (1.0 - cos(k * L) * cosh(k * L));
    
    KW(0,0)=fact * k * k * (cos(k * L)*sinh(k * L) + sin( k * L) * cosh (k * L));
    KW(0,1)=-fact * k * sin(k * L) * sinh(k * L);
    KW(0,2)=fact * k * k * (-sin(k * L) - sinh(k * L));
    KW(0,3)=-fact * k * (cosh(k * L) - cos(k * L));
    KW(1,0)=KW(0,1);
    KW(1,1)=fact * (sin(k * L) * cosh(k * L) - cos(k * L) * sinh(k * L));
    KW(1,2)=-KW(0,3);
    KW(1,3)=fact * (sinh(k * L) - sin(k * L));
    KW(2,0)=KW(0,2);
    KW(2,1)=KW(1,2);
    KW(2,2)=KW(0,0);
    KW(2,3)=-KW(0,1);
    KW(3,0)=KW(0,3);
    KW(3,1)=KW(1,3);
    KW(3,2)=KW(2,3);
    KW(3,3)=KW(1,1); 
    return KW;					      
}

MatrixXc XYRayleighBending(double w, double S, double Iz, double L,
                           double rho, cdouble EC){
        MatrixXc KW = MatrixXc::Zero(4,4);
        
        return KW;
}

MatrixXc XYTimoshenkoBending(double w, double S, double Iz, double L,
                             double rho, cdouble EC, double nu, double ky){
	    MatrixXc KW = MatrixXc::Zero(4,4);
	    cout << w << S << Iz << L << rho << EC << nu << ky << endl;
	    
        return KW;					   
}

MatrixXc StraightPlanarBeam(double w, double S, double Iz, double L, double rho,
                    double E, double eta, double nu, double ky, char Th){
	MatrixXc KW = MatrixXc::Zero(6,6), KT = MatrixXc::Zero(2,2),
	         KB = MatrixXc::Zero(4,4);
	
	cdouble EC(E,E*eta);
	
	int LTraction[2]={0,3}, LBending[4]={1,2,4,5}; 
	
	KT = Traction(w, S, L, rho, EC);
	
	if (Th == 'B')
	    KB = XYBending(w, S, Iz, L, rho, EC);
	else if(Th == 'R')
	    KB = XYRayleighBending(w, S, Iz, L, rho, EC);
	else if(Th == 'T')
	    KB = XYTimoshenkoBending(w, S, Iz, L, rho, EC, nu, ky);

    KW(LTraction,LTraction) = KT;
    KW(LBending, LBending) = KB;
    	
	return KW;
}

MatrixXc Straight3DBeam(double w, double S, double Iy, double Iz, double J, 
                        double L, double rho, double E, double G, double eta,
                        double nu, double ky, double kz, char Th){
							
	MatrixXc KW = MatrixXc::Zero(12,12), KT = MatrixXc::Zero(2,2),
	         KBXY = MatrixXc::Zero(4,4), KBXZ = MatrixXc::Zero(4,4),
	         KTO = MatrixXc::Zero(2,2);
	
	cdouble EC(E,E*eta), GC(G, G*eta);
	
	int LTraction[2]={0,6}, LBendingXY[4]={1,5,7,11},
	    LBendingXZ[4]={2,4,8,10}, LTorsion[2]={3,9}; 
	
	KT = Traction(w, S, L, rho, EC);
	KTO = Torsion(w, S, J, Iy + Iz, L, rho, GC);
	
	if (Th == 'B'){
	    KBXY = XYBending(w, S, Iz, L, rho, EC);
	    KBXZ = XZBending(w, S, Iy, L, rho, EC);
	}
/*	else if(Th == 'R')
	    KB = XYRayleighBending(w, S, Iz, L, rho, EC);
	else if(Th == 'T')
	    KB = XYTimoshenkoBending(w, S, Iz, L, rho, EC, nu, ky);
*/
    KW(LTraction,LTraction) = KT;
    KW(LTorsion, LTorsion) = KTO;
    KW(LBendingXY, LBendingXY) = KBXY;
    KW(LBendingXZ, LBendingXZ) = KBXZ;
  	
	return KW;
}

MatrixXc GlobalPlanarBeam(double w, double S, double Iz, double L,
                          double rho, double E, double eta, double nu,
                          double ky, double X[], char Th){
    MatrixXc K = MatrixXc::Zero(6, 6), P = MatrixXc::Zero(6,6),
             PT = MatrixXc::Zero(6,6);				
    		  
	MatrixXc KL = StraightPlanarBeam(w, S, Iz, L, rho, E, eta, nu, ky, Th);
	
	P(0,0)=X[0]/L;
    P(1,0)=X[1]/L;
    P(0,1)=-P(1,0);
    P(1,1)=P(0,0);
    P(2,2)=1;
    P(3,3)=P(0,0);
    P(4,3)=P(1,0);
    P(3,4)=P(0,1);
    P(4,4)=P(1,1);
    P(5,5)=1 ;
    
    K = P.transpose() * KL * P;  
//	cout << K.format(fmtMat) << endl;	
	return K;
}

MatrixXc Global3DBeam(double w, double S, double Iy, double Iz, double J,
                      double L,double rho, double E, double G, double eta,
                      double nu,double ky, double kz, double X[], 
                      double V[], char Th){
   
    MatrixXc K = MatrixXc::Zero(12, 12), P0 = MatrixXc::Zero(3,3),
             PT = MatrixXc::Zero(12,12), P = MatrixXc::Zero(12,12);				
    		  
	MatrixXc KL = Straight3DBeam(w, S, Iy, Iz, J, L, rho, E, G, eta, nu, ky, kz, Th);
	double NV = sqrt(V[0]*V[0]+V[1]*V[1]+V[2]*V[2]); 
		
	P0(0,0) = X[0] / L;
    P0(1,0) = X[1] / L;
    P0(2,0) = X[2] / L;
    P0(0,2) = P0(1,0) * V[2] / NV - P0(2,0) * V[1] / NV ;
    P0(1,2) = P0(2,0) * V[0] / NV - P0(0,0) * V[2] / NV ;
    P0(2,2) = P0(0,0) * V[1] / NV - P0(1,0) * V[0] / NV ;
    P0(0,1) = P0(1,2) * P0(2,0) - P0(1,0) * P0(2,2);
    P0(1,1) = P0(2,2) * P0(0,0) - P0(2,0) * P0(0,2);
    P0(2,1) = P0(0,2) * P0(1,0) - P0(0,0) * P0(1,2);
    	  
    P.block(0,0,3,3) = P0;
    P.block(3,3,3,3) = P0;
    P.block(6,6,3,3) = P0;
    P.block(9,9,3,3) = P0;
    
    K = P.transpose() * KL * P;  
//	cout << K.format(fmtMat) << endl;		
	return K;
}

MatrixXc PlanarAssembly(MatrixXc Kwst, MatrixXc Kw, Element e){
	int map[6] = {
            dof(e.node1, 0),
            dof(e.node1, 1),
            dof(e.node1, 2),
            dof(e.node2, 0),
            dof(e.node2, 1),
            dof(e.node2, 2)
        };

     for (int i = 0; i < 6; ++i)
         for (int j = 0; j < 6; ++j)
                Kwst(map[i], map[j]) += Kw(i, j);
	
    return Kwst;
}

MatrixXc ThreeDAssembly(MatrixXc Kwst, MatrixXc Kw, Element e, ofstream& out_def){
	int map[12] = {
            dof3D(e.node1, 0),
            dof3D(e.node1, 1),
            dof3D(e.node1, 2),
            dof3D(e.node1, 3),
            dof3D(e.node1, 4),
            dof3D(e.node1, 5),
            dof3D(e.node2, 0),
            dof3D(e.node2, 1),
            dof3D(e.node2, 2),
            dof3D(e.node2, 3),
            dof3D(e.node2, 4),
            dof3D(e.node2, 5)
        };
     
     for (int i = 0; i < 12; ++i)
         for (int j = 0; j < 12; ++j)
                Kwst(map[i], map[j]) += Kw(i, j);
	
    return Kwst;
}

MatrixXc DynamicStiffness2D(const Model& data, double omega, ofstream& out_def){
	int ndof = 3 * data.nodes.size();
    MatrixXc K = MatrixXc::Zero(ndof, ndof);
 
    double X[2];
    
    for(const auto& e : data.elements){
		const Node& n1 = data.nodes[e.node1-1];
        const Node& n2 = data.nodes[e.node2-1];
        const Material& mat = data.materials[e.material_id-1];
        const Section& sec = data.sections[e.section_id-1];
        double dx = n2.x - n1.x;
        double dy = n2.y - n1.y;
        X[0] = dx;
        X[1] = dy;
            
        double L  = std::sqrt(dx*dx + dy*dy);
        MatrixXc k_elem = GlobalPlanarBeam(omega, sec.A, sec.Iz, L,
                                           mat.rho, mat.E, mat.eta,
                                           mat.nu, sec.kappay, X, 'B');
        K = PlanarAssembly(K, k_elem, e);
        
    }
//    cout << K.format(fmtMat) << endl;	
    return K;
}

MatrixXc DynamicStiffness3D(const Model& data, double omega, ofstream& out_def){
	int ndof = 6 * data.nodes.size();
    MatrixXc K = MatrixXc::Zero(ndof, ndof);
    double X[3], V[3];
 
    for(const auto& e : data.elements){
		const Node& n1 = data.nodes[e.node1-1];
        const Node& n2 = data.nodes[e.node2-1];
        const Material& mat = data.materials[e.material_id-1];
        const Section& sec = data.sections[e.section_id-1];
        double dx = n2.x - n1.x;
        double dy = n2.y - n1.y;
        double dz = n2.z - n1.z;
        
        X[0] = dx;
        X[1] = dy;
        X[2] = dz;
        
        V[0]=e.V[0];
        V[1]=e.V[1];
        V[2]=e.V[2];
            
        double L  = std::sqrt(dx*dx + dy*dy + dz*dz);
        
        MatrixXc k_elem = Global3DBeam(omega, sec.A, sec.Iy, sec.Iz,
                                       sec.J, L,mat.rho, mat.E, mat.G, 
                                       mat.eta, mat.nu, sec.kappay,
                                       sec.kappaz, X, V, 'B');
        K = ThreeDAssembly(K, k_elem, e, out_def);
    }
//    cout << K.format(fmtMat) << endl;	
    return K;
}


Model read_model(const std::string& filename)
{
	int nddlnode;
    Model data;
    std::ifstream in(filename);
    if (!in)
        throw std::runtime_error("Impossible d'ouvrir " + filename);

//    DSMInput data;
    std::string keyword;

    // 1. Type de problème (ex: 2DFRAME)
    in >> data.problem_type;
    std::cout << "Problem type: " << data.problem_type << std::endl;
    
    if (data.problem_type == "2DFRAME")
        nddlnode = 3;
    else if (data.problem_type == "3DFRAME")
        nddlnode = 6;
        
    while (in >> keyword) {
		cout << "keyword" << keyword << endl;
        if (keyword == "NODES") {
            int n;
            in >> n;
            if (n <= 0 || n > 100000)
                throw std::runtime_error("Nombre de noeuds invalide");

            data.nodes.resize(n);
            if (data.problem_type == "2DFRAME"){
                for (int i = 0; i < n; ++i) {
                    in >> data.nodes[i].id
                       >> data.nodes[i].x
                       >> data.nodes[i].y;
                }
            }
            else if(data.problem_type == "3DFRAME"){
                for (int i = 0; i < n; ++i) {
                    in >> data.nodes[i].id
                       >> data.nodes[i].x
                       >> data.nodes[i].y
                       >> data.nodes[i].z;
                }
		    }
                
        }

        else if (keyword == "ELEMENTS") {
            int n;
            in >> n;
            if (n <= 0 || n > 100000)
                throw std::runtime_error("Nombre d'elements invalide");

            data.elements.resize(n);
            if (data.problem_type == "2DFRAME"){
                for (int i = 0; i < n; ++i) {
                    in >> data.elements[i].id
                       >> data.elements[i].node1
                       >> data.elements[i].node2
                       >> data.elements[i].material_id
                       >> data.elements[i].section_id;

                    int dummy;
                    in >> dummy; // dernier champ non utilisé
                }
             }
             else if(data.problem_type == "3DFRAME"){
                for (int i = 0; i < n; ++i) {
                    in >> data.elements[i].id
                       >> data.elements[i].node1
                       >> data.elements[i].node2
                       >> data.elements[i].material_id
                       >> data.elements[i].section_id
                       >> data.elements[i].V[0]
                       >> data.elements[i].V[1]
                       >> data.elements[i].V[2];
                }
             }
        }
        else if (keyword == "MATERIALS") {
            int n;
            in >> n;
            if (n <= 0 || n > 10000)
                throw std::runtime_error("Nombre de materiaux invalide");

            data.materials.resize(n);
            if (data.problem_type == "2DFRAME"){
                for (int i = 0; i < n; ++i) {
                    int id;
                    in >> id
                       >> data.materials[i].rho
                       >> data.materials[i].E
                       >> data.materials[i].eta
                       >> data.materials[i].nu;
                }
            }
            else if (data.problem_type == "3DFRAME"){
				for (int i = 0; i < n; ++i) {
                    int id;
                    in >> id
                       >> data.materials[i].rho
                       >> data.materials[i].E
                       >> data.materials[i].G
                       >> data.materials[i].eta
                       >> data.materials[i].nu;
                }
		    }
        }

        else if (keyword == "SECTIONS") {
            int n;
            in >> n;
            if (n <= 0 || n > 10000)
                throw std::runtime_error("Nombre de sections invalide");

            data.sections.resize(n);
            if (data.problem_type=="2DFRAME"){
               for (int i = 0; i < n; ++i) {
                   int id;
                   in >> id
                      >> data.sections[i].A
                      >> data.sections[i].Iz
                      >> data.sections[i].kappay;
               }
           }
           else if (data.problem_type=="3DFRAME"){
			   for (int i = 0; i < n; ++i) {
                   int id;
                   in >> id
                      >> data.sections[i].A
                      >> data.sections[i].Iy
                      >> data.sections[i].Iz
                      >> data.sections[i].J
                      >> data.sections[i].kappay
                      >> data.sections[i].kappaz;
               }
	       } 
        }
        else if (keyword == "FDOF") {
            int n;
            cout << "Keyword FDOF" << endl;
            in >> n;
            cout << "n=" << n << endl;
            if (n <= 0 || n > 10000)
                throw std::runtime_error("Nombre de ddl bloqués faux");

            data.fdof.resize(n);
            for (int i = 0; i < n; ++i) {
                int nd,dof;
                in >> nd >> dof;
                data.fdof[i] = (nd-1)*nddlnode + dof - 1;
            }
        }
        

        else {
            throw std::runtime_error("Mot-cle inconnu : " + keyword);
        }
    }

    return data;

}


VectorXc solve_dsm(const MatrixXc& K, const VectorXc& F){
    return K.fullPivLu().solve(F);
}


 MatrixXc reduce_matrix(const MatrixXc& K, const std::vector<int>& free_dofs){
    int n = free_dofs.size();
    MatrixXc Kr(n, n);

    for (int i = 0; i < n; ++i)
        for (int j = 0; j < n; ++j)
            Kr(i,j) = K(free_dofs[i], free_dofs[j]);
    return Kr;
}

VectorXc reduce_vector(const VectorXc& F, const std::vector<int>& free_dofs){
    int n = free_dofs.size();
    VectorXc Fr(n);

    for (int i = 0; i < n; ++i)
        Fr(i) = F(free_dofs[i]);

    return Fr;
}

VectorXc reconstruct_solution(const VectorXc& U_r,const std::vector<int>& free_dofs,int ndof){
    VectorXc U = VectorXc::Zero(ndof);

    for (size_t i = 0; i < free_dofs.size(); ++i)
        U(free_dofs[i]) = U_r(i);

    return U;
}


//25/03 std::vector<std::complex<double>> run_dsm(const std::string& filename, int exc_node, int exc_dof, int obs_node, int obs_dof,
//25/03                                          double fmin, double fmax, int npts, double fdef){

VectorXc run_dsm(const std::string& filename, int exc_node, int exc_dof, int obs_node, int obs_dof,
                                                     double fmin, double fmax, int npts, double fdef){
											      

    bool def_ok = false;
    
    

    Model data = read_model(filename);
 
    cout << "Nodes: " << data.nodes.size() << endl;
    cout << "Elements: " << data.elements.size() << endl;
    cout << "Materials: " << data.materials.size() << endl;
    cout << "Sections: " << data.sections.size() << endl;
    cout << "fmin : " << fmin << endl;
    cout << "fmax : " << fmax << endl;
    cout << "Nombre de points de calcul : " << npts << endl;
    cout << "frequence de la deformee" << fdef << endl;
    cout << "Nombre de ddl bloques : " << data.fdof.size() << endl;
   
//25/03    std::vector<std::complex<double>> U_py;
//25/03 bis    Eigen::VectorXd U_amp(npts) ; 
    VectorXc U_vec(npts);
    
    MatrixXc K;  
    double df = (fmax - fmin)/(npts-1);
    int nddlnode = (data.problem_type=="2DFRAME") ? 3 : 6;
    
    
    MatrixXc UDEF = MatrixXc::Zero(data.nodes.size(), 6);
    
    int ndof = nddlnode * data.nodes.size();
 
    Eigen::VectorXcd F = Eigen::VectorXcd::Zero(ndof);
    
    
    int g_exc = nddlnode * exc_node + exc_dof;
    
    F(g_exc) = std::complex<double>(1.0, 0.0);
    
    cout << "effort" << endl;
    for(int i=0; i<F.size();i++)
       cout << F(i) << endl;
   

    int g_obs = nddlnode * obs_node + obs_dof;

    std::vector<int> free_dofs;

    std::vector<bool> is_fixed(ndof, false);
    cout << "fixed dof" << endl;
    
    for (int d : data.fdof){
	   cout << d << endl;
       is_fixed[d] = true;
    }



    for (int i = 0; i < ndof; ++i)
       if (!is_fixed[i])
          free_dofs.push_back(i);
    
    cout << "ddl libres" << endl;
    for(int i = 0; i < (int)free_dofs.size();i++)
        cout << free_dofs[i] << endl;
        
    std::ofstream out("response.dat");
    out << "# f(Hz)   |U|\n";
    
    ofstream out_def("displacements.dat");
    
// 05/03    U_py.reserve(npts);
    
    int i=0;
    for (double f = fmin; f <= fmax; f += df,i++) {
		
       cout << "f=" << f << endl;
       double omega = 2.0 * M_PI * f;

       
       if(data.problem_type=="2DFRAME")
           K = DynamicStiffness2D(data,omega,out_def);
       else if(data.problem_type=="3DFRAME")
           K = DynamicStiffness3D(data,omega,out_def);
           
       
       MatrixXc K_r = reduce_matrix(K, free_dofs);
       VectorXc F_r = reduce_vector(F, free_dofs);
      
       VectorXc U_r = K_r.fullPivLu().solve(F_r);
       VectorXc U   = reconstruct_solution(U_r, free_dofs, ndof);
       
       
//       if(i==1){
//		   
//		   std::cout << omega << std::endl;
//		   cout << K.format(fmtMat) << endl;
//		   std::cout << "U" << std::endl;
//		   cout << U.format(fmtVec) << endl;
//		   for(int j=0; j<free_dofs.size();j++)
//		       cout << free_dofs[j] << endl;
//           std::cout << K_r.format(fmtMat) << std::endl;
//       }


// 05/03        double amp = std::abs(U(g_obs));
// 05/03 bis      U_amp(i) = std::abs(U(g_obs));   
         U_vec(i) = U(g_obs);

//       std::complex<double> response = U(g_obs);
// 05/03       out << f << " " << amp << "\n";
// 05/03 bis   out << f << " " << U_amp(i) << "\n";
         out << f << " " << U_vec(i) << "\n";
//05/03        U_py.push_back(amp);
       
//       if(data.problem_type=="2DFRAME")
//          cout << K.block(3,3,3,3).format(fmtMat)<<endl;
//       else if(data.problem_type=="3DFRAME")
//          cout << K.block(6,6,6,6).format(fmtMat)<<endl;     
       if(!def_ok && fdef<f){
//		   cout << K.format(fmtMat) << endl;
		   def_ok=true;
		   for(int ndef=1;ndef<=data.nodes.size();ndef++){
		       for(int ddldef=0;ddldef<=nddlnode-1;ddldef++)
		          out_def << real(U((ndef-1)*nddlnode + ddldef)) << ' ';
		       out_def << endl;
		   }
		   out_def.close();
	   }	   
   }

//   for (const auto& u : U_py)
//    std::cout << u << std::endl;
   out.close();
   
// 05/03    return U_py;
// 05/03 bis   return U_amp;
   return U_vec;
}


