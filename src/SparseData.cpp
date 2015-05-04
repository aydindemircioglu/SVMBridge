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
static char buffer [33];

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
List readSparseData (std::string filename, bool verbose = false, bool zeroBased = false) {
	try
	{
		
		int correction = 1;
		if (zeroBased == true)
			correction = 0;
		
		// determine size
		int index = 0;
		int max_index = 0;
		int inst_max_index = 0;
		int i = 0;
		FILE *fp = fopen(filename.c_str(),"r");
		char *endptr = NULL;
		char *idx = NULL;
		char *val = NULL;
		char *label = NULL;
		char *p = NULL;
		stringstream s;
		
		if(fp == NULL)
		{
			s << "Can't open input file " << filename;
			::Rf_error(s.str().c_str());
			return R_NilValue;
		}

		int l = 0;
		int featureDimension = 0;
		
		max_line_len = 1024;
		line = Malloc(char,max_line_len);
		index = 0;
		max_index = 0;
		
// 		if(readline(fp) != NULL)
// 		{
// 			p = strtok(line," \t\n"); // label
// 			rewind(fp);
// 		}
		
		while(readline(fp)!=NULL)  //global variable line updated
		{
			p = strtok(line," \t"); // label
			inst_max_index = -1;
			//label = strtok(line," \t");
			strtod(p,&endptr);
			
			if(p == NULL) // empty line
				break;
			
			if(endptr == p || *endptr != '\0')
				break;
			
			// features
			while(1)
			{
				index = 0;
				idx = strtok(NULL,":");
				p = strtok(NULL," \t"); 
				
				if(p == NULL || *p == '\n') {  //CURRENT FAILURE
					break;
				}
				
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
				int c = strtod(p,&endptr); // x:y .. we dont need the right side for this step, which is defined by p
				
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
		
		n=l;
		m=max_index;
		DEBUG printf("Max_Index: %d \n", max_index); 
		featureDimension = max_index + 1 - correction;
		sprintf(buffer, "%d", max_index);
		
		if (verbose == true) 
			printf("Found data dimensions: %d x %d\n", l, featureDimension);

		
		// resize
		//line = Malloc(char,max_line_len);
		rewind(fp);
		Rcpp::NumericMatrix xR(l,featureDimension);
		Rcpp::NumericVector yR(l);
		endptr = NULL;
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
			DEBUG printf("yR[%d] = %f\n", i, yR[i]);
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
				index = (int) strtol(idx,&endptr,10); 
				
				if(endptr == idx || errno != 0 || *endptr != '\0' || index <= inst_max_index)
				{
					s << "Error in endptr " ;
					::Rf_error(s.str().c_str());
					return R_NilValue;
				}
				else
					inst_max_index = index;

				errno = 0;
				xR(i, index - correction) = strtod(val,&endptr);
				DEBUG printf("xR[%d, %d] = %f\n", i, index, xR(i, index));
				
				if(endptr == val || errno != 0 || (*endptr != '\0' && !isspace(*endptr)))
				{
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
	printf("Done Reading");
} 





//' @export
// [[Rcpp::export]] 
List writeSparseData (std::string filename, NumericMatrix x, NumericVector y, bool verbose = false, bool zeroBased = false) {
	
	try
	{
		stringstream s;

		std::fstream fs;
		fs.open(filename.c_str(), std::fstream::in | std::fstream::out |std::fstream::trunc);
		
		int correction = 0;
		if (zeroBased == false) {
			correction = 1;
		}

		for(int i=0;i<n;i++) 
		{
			fs << y(i) << " ";
			for(int j=0;j<m;j++)
			{
				fs << j + correction << ":" << x(i,j) << " ";
			}
			fs << "\n";
		}
		
		DEBUG printf("Done Writing");
		fs.close();
	}
	catch(int e)
	{
		printf("Error: %d", e);
	}
	return R_NilValue;
}

