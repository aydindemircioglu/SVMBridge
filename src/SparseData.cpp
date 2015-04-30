// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
//
// SVMBridge
// 	-- readSparse


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

#include <Rcpp.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sstream>
#include <fstream>
#include <iostream>
#define Malloc(type,n) (type *)malloc((n)*sizeof(type))
#define Calloc(type,n) (type *)calloc(n, sizeof(type))

#define DEBUG if (1 == 0) 

using namespace std;
using namespace Rcpp;

static char *line = NULL;
static int n=0;
static int m=0;
static int max_line_len;

static char* readline(FILE *input)
{
	int len;
	
	if(fgets(line,max_line_len,input) == NULL)
		return NULL;

	while(strrchr(line,'\n') == NULL)
	{
		max_line_len *= 2;
		line = (char *) realloc(line,max_line_len);
		len = (int) strlen(line);
		if(fgets(line+len,max_line_len-len,input) == NULL)
			break;
	}
	return line;
}


//' @export
// [[Rcpp::export]] 
RcppExport SEXP readSparseData (SEXP filename, SEXP parameter) {
	try
	{
		Rcpp::List rparam(parameter);
		bool _verbose = false;
		if (rparam.containsElementNamed("verbose") == true) {
			_verbose = Rcpp::as<bool>(rparam["verbose"]);
		} 
		bool _zeroBased = false;
		if (rparam.containsElementNamed("zeroBased") == true) {
			_zeroBased = Rcpp::as<bool>(rparam["zeroBased"]);
		}
		
		std::string _filename = Rcpp::as<string>(filename);
		int correction = 1;
		if (_zeroBased == true)
			correction = 0;
		
		// determine size
		int index = 0;
		int max_index = 0;
		int inst_max_index = 0;
		int i = 0;
		int l = 0;
		int featureDimension = 0;
		FILE *fp = fopen(_filename.c_str(),"r");
		char *endptr = NULL;
		char *idx = NULL;
		char *val = NULL;
		char *label = NULL;
		char *p = NULL;
		stringstream s;
		
		if(fp == NULL){
			s << "Can't open input file " << _filename;
			::Rf_error(s.str().c_str());
			return R_NilValue;
		}
		
		max_line_len = 1024;
		line = Malloc(char,max_line_len);
		
		//Get highest feature dimension and number of lines of chosen dataset
		while(readline(fp)!=NULL)  //global variable line updated
		{
			p = strtok(line," \t"); 
			inst_max_index = -1;
			strtod(p,&endptr);
			
			if(p == NULL) // empty line
				break;
			
			if(endptr == p || *endptr != '\0')
				break;
		
			while(1)
			{
				index = 0;
				idx = strtok(NULL,":");
				p = strtok(NULL," \t"); 
				
				if(p == NULL || *p == '\n')  //CURRENT FAILURE
					break;
				
				errno = 0;
				index = (int) strtol(idx,&endptr,10); 
				
				if(endptr == idx || errno != 0 || *endptr != '\0' || index <= inst_max_index) {
					s << "Error while loading file. " ;
					::Rf_error(s.str().c_str());
					return R_NilValue;
				}
				else
					inst_max_index = index;
				
				errno = 0;
				strtod(p,&endptr); 
				
				if(endptr == p || errno != 0 || (*endptr != '\0' && !isspace(*endptr))){
					s << "Input Error" ;
					::Rf_error(s.str().c_str());
					return R_NilValue;
				}
				
				if (index > max_index)
					max_index = index;
			}
			++l;
		}
		rewind(fp);
		
		featureDimension = max_index;
		n=l; //set global variable n for function writeSparseData
		m=max_index; //set global variable m for function writeSparseData
		
		if (_verbose == true) 
			printf("Found data dimensions: %d x %d\n", l, featureDimension);

		Rcpp::NumericMatrix xR(l,featureDimension);
		Rcpp::NumericVector yR(l);
		endptr = NULL;
		
		//Fill NumericMatrix xR, NumericVector yR
		for (int i=0; i<l; i++)
		{
			inst_max_index = -1; 
			readline(fp);
			label = strtok(line," \t\n");
			
			if(label == NULL) {
				s << "Error: label = 0 " ;
				::Rf_error(s.str().c_str());
				return R_NilValue;
			}
			
			yR[i] = strtod(label, &endptr);
			if(endptr == label || *endptr != '\0'){
					s << "Error in endptr " ;
					::Rf_error(s.str().c_str());
					return R_NilValue;
				}
			
			while(1)
			{
				idx = strtok(NULL,":");
				val = strtok(NULL," \t");
				if(val == NULL) //No further informations in this line.. end of line
					break;
		
				errno = 0;
				index = (int) strtol(idx,&endptr,10); //errno may be updated in this step
				
				if(endptr == idx || errno != 0 || *endptr != '\0' || index <= inst_max_index){
					s << "Error in endptr " ;
					::Rf_error(s.str().c_str());
					return R_NilValue;
				}
				else
					inst_max_index = index;

				errno = 0;
				xR(i, index - correction) = strtod(val,&endptr); //errno may be updated in this step
				
				if(endptr == val || errno != 0 || (*endptr != '\0' && !isspace(*endptr))){
					s << "Error in endptr " ;
					::Rf_error(s.str().c_str());
					return R_NilValue;
				}
			}
			if(inst_max_index > max_index)
				max_index = inst_max_index;
		}
		
		fclose(fp);
		free(line);
		Rcpp::List rl = Rcpp::List::create (Rcpp::Named ("X", xR), Rcpp::Named("Y", yR) );
		return (rl);
	}	
	
	catch(int e)
	{
		printf("Error: %d", e);
		exit(1);
	}
} 


//' @export
// [[Rcpp::export]] 
RcppExport SEXP writeSparseData (SEXP x, SEXP y, SEXP parameter) {
	
	try
	{
		stringstream s;

		Rcpp::List rparam(parameter);
		
		bool _verbose = false;
		if (rparam.containsElementNamed("verbose") == true) {
			_verbose = Rcpp::as<bool>(rparam["verbose"]);
		} 
		
		bool _zeroBased = false;
		if (rparam.containsElementNamed("zeroBased") == true) {
			_zeroBased = Rcpp::as<bool>(rparam["zeroBased"]);
		}
		
		if (rparam.containsElementNamed("filename") == false) {
			s << "Please specify a filename." ;
			::Rf_error(s.str().c_str());
			return R_NilValue;
		}
		
		printf("a\n");
		std::string _filename = Rcpp::as<string>(rparam["filename"]);
		printf("b\n");
		Rcpp::NumericMatrix xR = Rcpp::NumericMatrix(x);
		printf("c\n");
		Rcpp::NumericVector yR = Rcpp::NumericVector(y);
		printf("d\n");
		std::fstream fs;
		fs.open(_filename.c_str(), std::fstream::in | std::fstream::out |std::fstream::trunc);
		
		int correction = 0;
		if (_zeroBased == false) {
			correction = 1;
		}

		for(int i=0;i<n;i++) 
		{
			fs << yR(i) << " ";
			for(int j=0;j<m;j++)
			{
				fs << j + correction << ":" << xR(i,j) << " ";
			}
			fs << "\n";
		}
		fs.close();
	}
	catch(int e)
	{
		printf("Error: %d", e);
	}
	return R_NilValue;
}

