// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
//
// SVMBridge
// 	-- OptimizationValues


//
// Copyright (C) 2015  Aydin Demircioglu, aydin.demircioglu /at/ ini.rub.de
//
// This file is part of the SwarmSVM library for GNU R.
// It is made available under the terms of the GNU General Public
// License, version 2, or at your option, any later version,
// incorporated herein by reference.
//
// This program is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied
// warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public
// License along with this program; if not, write to the Free
// Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA

#include <stdlib.h>
#include <Rcpp.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sstream>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <string>
#ifdef __linux
	#include <unistd.h>
	#include <sys/types.h>
	#include <pwd.h>
#endif


#define DEBUG if (1 == 0) 

#define Malloc(type,n) (type *)malloc((n)*sizeof(type))


using namespace std;
using namespace Rcpp;

typedef double Qfloat;


struct svm_node
{
	int index;
	double value;
};


enum { C_SVC, NU_SVC, ONE_CLASS, EPSILON_SVR, NU_SVR }; /* svm_type */
enum { LINEAR, POLY, RBF, SIGMOID, PRECOMPUTED }; /* kernel_type */

struct svm_parameter
{
	int svm_type;
	int kernel_type;
	int degree;     /* for poly */
	double gamma;   /* for poly/rbf/sigmoid */
	double coef0;   /* for poly/sigmoid */
	
	/* these are for training only */
	double cache_size; /* in MB */
	double eps;     /* stopping criteria */
	double C;       /* for C_SVC, EPSILON_SVR and NU_SVR */
	int nr_weight;          /* for C_SVC */
	int *weight_label;      /* for C_SVC */
	double* weight;         /* for C_SVC */
	double nu;      /* for NU_SVC, ONE_CLASS, and NU_SVR */
	double p;       /* for EPSILON_SVR */
	int shrinking;  /* use the shrinking heuristics */
	int probability; /* do probability estimates */
};


static inline double powi(double base, int times)
{
	double tmp = base, ret = 1.0;
	
	for(int t=times; t>0; t/=2)
	{
		if(t%2==1) ret*=tmp;
		tmp = tmp * tmp;
	}
	return ret;
}


template <class S, class T> static inline void clone(T*& dst, S* src, int n)
{
	dst = new T[n];
	memcpy((void *)dst,(void *)src,sizeof(T)*n);
}



//
// Kernel evaluation
//
// the static method k_function is for doing single kernel evaluation
// the constructor of Kernel prepares to calculate the l*l kernel matrix
// the member function get_Q is for getting one column from the Q Matrix
//
class QMatrix {
public:
	virtual Qfloat *get_Q(int column, int len) const = 0;
	virtual double *get_QD() const = 0;
	virtual void swap_index(int i, int j) const = 0;
	virtual ~QMatrix() {}
	
	virtual const svm_node * get_x(int i) const = 0;
};

class Kernel: public QMatrix {
public:
	Kernel(int l, svm_node * const * x, const svm_parameter& param);
	virtual ~Kernel();
	
	static double k_function(NumericVector x, NumericVector y, double gamma);
	//static double k_function_sparse(const svm_node *x, const svm_node *y, const svm_parameter& param);
	virtual Qfloat *get_Q(int column, int len) const = 0;
	virtual double *get_QD() const = 0;
	virtual void swap_index(int i, int j) const	// no so const...
	{
		swap(x[i],x[j]);
		if (x_square) swap(x_square[i],x_square[j]);
	}
	
	virtual const svm_node * get_x(int i) const
	{ return x[i]; }
	
protected:
	
	double (Kernel::*kernel_function)(int i, int j) const;
	
private:
	const svm_node **x;
	double *x_square;
	
	// svm_parameter
	const int kernel_type;
	const int degree;
	const double gamma;
	const double coef0;
	
	static double dot(const svm_node *px, const svm_node *py);
	double kernel_linear(int i, int j) const
	{
		return dot(x[i],x[j]);
	}
	double kernel_poly(int i, int j) const
	{
		return powi(gamma*dot(x[i],x[j])+coef0,degree);
	}
	double kernel_rbf(int i, int j) const
	{
		return exp(-gamma*(x_square[i]+x_square[j]-2*dot(x[i],x[j])));
	}
	double kernel_sigmoid(int i, int j) const
	{
		return tanh(gamma*dot(x[i],x[j])+coef0);
	}
	double kernel_precomputed(int i, int j) const
	{
		return x[i][(int)(x[j][0].value)].value;
	}
};


Kernel::Kernel(int l, svm_node * const * x_, const svm_parameter& param)
:kernel_type(param.kernel_type), degree(param.degree),
gamma(param.gamma), coef0(param.coef0)
{
	switch(kernel_type)
	{
		case LINEAR:
			kernel_function = &Kernel::kernel_linear;
			break;
		case POLY:
			kernel_function = &Kernel::kernel_poly;
			break;
		case RBF:
			kernel_function = &Kernel::kernel_rbf;
			break;
		case SIGMOID:
			kernel_function = &Kernel::kernel_sigmoid;
			break;
		case PRECOMPUTED:
			kernel_function = &Kernel::kernel_precomputed;
			break;
	}
	
	clone(x,x_,l);
	
	if (kernel_type == RBF)
	{
		x_square = new double[l];
		for(int i=0;i<l; i++)
			x_square[i] = dot(x[i],x[i]);
	}
	else
		x_square = 0;
}


Kernel::~Kernel()
{
	delete[] x;
	delete[] x_square;
}


double Kernel::dot(const svm_node *px, const svm_node *py)
{
	double sum = 0;
	while(px->index != -1 && py->index != -1)
	{
		if (px->index == py->index)
		{
			sum += px->value * py->value;
			++px;
			++py;
		}
		else
		{
			if (px->index > py->index)
				++py;
			else
				++px;
		}			
	}
	return sum;
}



double Kernel::k_function(NumericVector x, NumericVector y, double gamma)
{
	NumericVector tmp = (x - y);
	double sum = std::inner_product(tmp.begin(), tmp.end(), tmp.begin(), 0.0);
	return exp (-gamma * sum);
}

/*
double Kernel::k_function_sparse(NumericVector x, NumericVector y, double gamma)
{
	double sum = 0;
	while(x->index != -1 && y->index !=-1)
	{
		if (x->index == y->index)
		{
			double d = x->value - y->value;
			sum += d*d;
			++x;
			++y;
		}
		else
		{
			if (x->index > y->index)
			{	
				sum += y->value * y->value;
				++y;
			}
			else
			{
				sum += x->value * x->value;
				++x;
			}
		}
	}
	
	while(x->index != -1)
	{
		sum += x->value * x->value;
		++x;
	}
	
	while(y->index != -1)
	{
		sum += y->value * y->value;
		++y;
	}
	
	return exp(-param.gamma*sum);
}*/



double svm_predict_values(double gamma, NumericVector x, NumericMatrix SV, NumericVector nSV, NumericMatrix sv_coef, NumericVector rho, NumericVector label)
{
	int nr_class = nSV.length();
	int l = SV.nrow();
	
	double* dec_values = Malloc(double, nr_class * nr_class);
	
	double *kvalue = Malloc(double, l);
	for (int i = 0; i < l; i++) {
		kvalue[i] = Kernel::k_function(x, SV(i, _), gamma);
	}
	
	int *start = Malloc (int, nr_class);
	start [0] = 0;
	for (int i = 1; i < nr_class; i++)
		start [i] = start [i-1] + nSV [i-1];
	
	int *vote = Malloc (int, nr_class);
	for (int i = 0; i <nr_class; i++)
		vote[i] = 0;
	
	int p=0;
	for (int i = 0; i <nr_class; i++) 
	{
		for(int j=i+1;j<nr_class;j++)
		{
			double sum = 0;
			int si = start[i];
			int sj = start[j];
			int ci = nSV[i];
			int cj = nSV[j];
			
			int k;
			for (k=0; k<ci; k++) {
				sum += sv_coef (si+k, j-1) * kvalue[si+k];
			}
			for (k = 0; k < cj; k++) {
				sum += sv_coef (sj+k, i)* kvalue[sj+k];
			}
			sum -= rho[p];
			dec_values[p] = sum;
			
			if (dec_values[p] > 0)
				++vote[i];
			else
				++vote[j];
			p++;
		}
	}
	
	int vote_max_idx = 0;
	for(int i = 1; i < nr_class; i++) {
		if (vote[i] > vote[vote_max_idx])
			vote_max_idx = i;
	}

	double ret = dec_values[0];
	free(kvalue);
	free(start);
	free(vote);
	free(dec_values);

	// FiXME:
	// multiclass is again unclear
	
//	return label [vote_max_idx];
	return (ret);
}



//' Compute several optimization values from a model like primal value
//'
//' @param 	X		input data
//' @param		Y		labels
//' @param		C		regularization value from svm problem
//' @param		gamma	used kernel bandwidth 
//' @param		SV		matrix of support vectors
//' @param		nSV		vector of number of support vectors, needed for the multiclass case
//' @param		alpa		alpha of support vectors
//' @param		rho		vector of offset/bias terms
//' @param		label		vector of label mapping
//' @note 		works with LIBSVMlike models only
//' @note		all computations will be done in DENSE format, not sparse!
//' @return		list of optimization values
//' @export
// [[Rcpp::export]] 
Rcpp::List computeOptimizationValues (NumericMatrix X, NumericVector Y, double C, double gamma, NumericMatrix SV, NumericVector nSV, NumericMatrix alpha, NumericVector rho, NumericVector label, bool verbose = false)
{	
	double primal = -42;
	double dual = -42;
	double trainingError = 0;
	NumericVector trainingPredictions(X.nrow());
	double weight = 0;
	
	// silently enlarge matrix
	if (SV.ncol() != X.ncol ()) {
		if (verbose == TRUE)
			Rcout << "Dimensions of X and SV do not match.\n";
		if (SV.ncol() > X.ncol()) {
			NumericMatrix tmpX (X.nrow(), SV.ncol());
			// better copy possible?
			for (size_t i = 0; i < X.nrow(); i++) {
				NumericMatrix::Row zzrow = X( i, _);
				NumericMatrix::Row yyrow = tmpX( i, _);
				std::copy (zzrow.begin(), zzrow.end(), yyrow.begin());
			}
			X = tmpX;
		} else {
			NumericMatrix tmpSV (SV.nrow(), X.ncol());
			for (size_t i = 0; i < SV.nrow(); i++) {
				NumericMatrix::Row zzrow = SV( i, _);
				NumericMatrix::Row yyrow = tmpSV( i, _);
				std::copy (zzrow.begin(), zzrow.end(), yyrow.begin());
			}
			SV = tmpSV;
		}
	}
	
	try
	{
		int l = X.nrow();
		
		double hingeLoss = 0.0;
		for (int i = 0; i < l; i++)
		{
			// predict value
			double currentMarginO = svm_predict_values (gamma, X(i,_), SV, nSV, alpha, rho, label);
			double currentMargin = 1.0 - double(Y[i]) * currentMarginO; 
			if (currentMargin > 0)
				hingeLoss += currentMargin;
			if ((currentMarginO <= 0.0) && (Y[i] > 0))
				trainingError += 1;
			if ((currentMarginO > 0.0) && (Y[i] < 0))
				trainingError += 1;
			//			std::cout << currentMargin << "\n";
		}
		
		if (verbose == true) 
			Rcout << "Current hingeLoss: " <<  hingeLoss << "\n";
		
		// first term
		primal = C * hingeLoss;
		
		// compute weight squared
		int m =SV.nrow();
		for(int i = 0; i < m; i++)
		{
			for(int j = i; j < m; j++)
			{
				double k = Kernel::k_function (SV(i, _), SV(j, _), gamma);
				k = alpha (i,0) * k * alpha (j,0);
				weight += k;
				if (j != i)
					weight += k;
			}
		}
		weight = weight/2.0;
		
		// second term
		primal += weight;
		
		// compute dual
		dual = -weight;
		
		for (int j = 0; j < m; j++) {
			dual += fabs(alpha(j,0));
		}
		
		// output 
		if (verbose == true) 
			Rcout << "Current weight (0.5*||w||): " <<  sqrt(2*weight) << "\n";
		if (verbose == true) 
			Rcout << "Training error (" << trainingError << "/" << l << "): " << trainingError/X.nrow() << "\n";
		if (verbose == true) 
			Rcout <<  "Computed primal value: " << primal << "\n";
		if (verbose == true) 
			Rcout << "Computed dual value: " << dual << "\n";

		// fix training error
		trainingError = trainingError/X.nrow();
		
	}
	catch(int e)
	{
		stringstream s;
		s << "Unknown Error: " << e << "\n";
		::Rf_error(s.str().c_str());
	}
	
	Rcpp::List rl = Rcpp::List::create (
		Rcpp::Named ("trainingError", trainingError), 
		Rcpp::Named ("weight", weight), 
		Rcpp::Named ("primal", primal), 
		Rcpp::Named("dual", dual) );
	return (rl);
}


