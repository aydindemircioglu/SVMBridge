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

#include <stdlib.h>
#include <Rcpp.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sstream>
#include <fstream>
#include <iostream>
#include <iomanip>      // std::setprecision
#include <string.h>


#define Malloc(type,n) (type *)malloc((n)*sizeof(type))
#define Calloc(type,n) (type *)calloc(n, sizeof(type))

#define DEBUG if (1 == 0) 

using namespace std;
using namespace Rcpp;

static char *line = NULL;
static int n=0;
static int m=0;
static int max_line_len;


// TODOs: every X lines call Rcpp::checkUserInterrupt().
// use Rcout ifnstead of cout, if applicable.
// add .onUnload <- function (libpath) {
// library.dynam.unload("mypackage", libpath)
// } 
// somewhere
//

// internal helper function to read a line into global variable
//



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



//' Read a given file in sparse (LIBSVM) format to dense R matrix and R vector.
//'
//' @param 		filename		the filename of the data in sparse file format. The Tilde Character isn't supported for f filename!  
//' @param		verbose		show verbose output?
//' @param		zeroBased	do the indices in the file start with 0, e.g. -1 0:2 1:4 ...?
//' @keywords	IO 
//' @note		this routine is nearly a 1:1 adoptation from the LIBSVM original code.
//' @return		the data is read into an R matrix and an R vector, containing the data
//'					and the labels. note, that these are not in sparse format, but are dense.
//' @examples	
//' \dontrun{
//'		S = readSparseData("../../../SVMBridge/tests/data/australian.train")
//'		print (paste("Data has", nrow(S$X), "points."))
//'		print (paste("Labels are", unique(S$Y), "."))
//'	}
//' @export
// [[Rcpp::export]] 
List readSparseData (std::string filename, size_t skipBytes = 0, bool verbose = false, bool zeroBased = false) {
	
	try
	{	
		int correction = 1;
		if (zeroBased == true)
			correction = 0;
		
		int index = 0;
		int max_index = 0;
		int inst_max_index = 0;
		int i = 0;
		int min_index = 2;
		int l = 0;
		int featureDimension = 0;
		int class_number = 0;
		int class_count = 0;
		
		FILE *fp = fopen(filename.c_str(),"r");
		//Jump to position in dataset where sparse data starts
		fseek(fp, skipBytes, SEEK_CUR);
		
		
		char *endptr = NULL;
		char *idx = NULL;
		char *val = NULL;
		char *label = NULL;
		char *p = NULL;
		int class_members[10] = {0,0,0,0,0,0,0,0,0,0};
		
		stringstream s;
		
		if(fp == NULL){
			s << "Can't open input file " << filename;
			::Rf_error(s.str().c_str());
			return R_NilValue;
		}
		
		max_line_len = 1024;
		line = Malloc(char,max_line_len);
		
		//determine size
		while(readline(fp)!=NULL)  //global variable line updated
		{	
			p = strtok(line," \t"); 
			inst_max_index = -1;
			class_number  = strtod(p,&endptr);
			cout << "class " << class_number << endl;
			//determine if dataset contains multiclass data and get their class count
			if(class_number > class_count)
				class_count = class_number;
			if(class_number > 0){
				class_members[class_number]++;
				
			}
			
			
			if(p == NULL) // empty line
				break;
			
			if(endptr == p || *endptr != '\0')
				break;
		
			while(1)
			{
				index = 0;
				idx = strtok(NULL,":");
				p = strtok(NULL," \t"); 
				
				if(p == NULL || *p == '\n') 
					break;
				
				errno = 0;
				index = (int) strtol(idx,&endptr,10); 
				
				if(index < min_index)
					min_index = index;
				
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
		
		for( i=0;i<10;i++){
		  cout << "i: " << i << "class member: " << class_members[i] << endl;
		}
		
		if(zeroBased == false && min_index == 0){
			s << "ZeroBased is set to FALSE, dataset seems to start with zero\n"; 
			Rcpp::stop(s.str().c_str());
		}
		
		if(verbose == true){
			if(zeroBased == true && min_index == 1){
				Rcout << "Warning: zeroBased is set to TRUE, dataset seems to start with one\n";  
			}
		}
		rewind(fp);
		fseek(fp, skipBytes, SEEK_SET);
		
		
		DEBUG Rcout << "Max_Index: " <<  max_index << "\n"; 
		featureDimension = max_index + 1 - correction;

		n=l; //set global variable n for function writeSparseData
		m=max_index; //set global variable m for function writeSparseData
		
		if (verbose == true) 
			Rcout << "Found data dimensions: " << l << " x " << featureDimension << "\n";
		
		if(class_count > 1)
			if (verbose == true)
				Rcout << "Found multiclass data: " << class_count << " classes\n";
				
		
		Rcpp::NumericMatrix xR(l,featureDimension);
		Rcpp::NumericVector yR(l);
		endptr = NULL;
		
		if(class_count > 1){
		  
			int t = class_members[1]; 
			int r = class_members[2];
			int g = class_members[3];
			int h = class_members[4];
			int j = class_members[5];
			
			
			Rcpp::NumericMatrix xR1(t,featureDimension);
			Rcpp::NumericVector yR1(t);
			
			Rcpp::NumericMatrix xR2(r,featureDimension);
			Rcpp::NumericVector yR2(r);
			
			Rcpp::NumericMatrix xR3(g,featureDimension);
			Rcpp::NumericVector yR3(g);
			
			Rcpp::NumericMatrix xR4(h,featureDimension);
			Rcpp::NumericVector yR4(h);
			
			Rcpp::NumericMatrix xR5(j,featureDimension);
			Rcpp::NumericVector yR5(j);
			
			t = 0;
			r = 0;
			g = 0;
			h = 0;
			j = 0;
			
			//Fill NumericMatrix xR, NumericVector yR
			for ( i=0; i<l; i++)
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
				int value = yR(i);
				switch(value){
					case 1:{
						yR1[t] = yR(i);
						//cout << "t" << t << endl;
						t=t+1;break;
					}
					case 2:{
						yR2[r] = yR(i);
						//cout << "r" << r << endl;
						r=r+1;break;
					}
					case 3:{
						yR3[g] = yR(i);
						//cout << "g" << g << endl;
						g=g+1;break;
					}
					case 4:{
						yR4[h] = yR(i);
						//cout << "h" << h << endl;
						h=h+1;break;
					}
					case 5:{
						yR5[j] = yR(i);
						//cout << "j" << j << endl;
						j=j+1;break;
					}
					default:{break;}
				    
				}
				
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
					
					switch(value){
						case 1:{
							xR1(t-1, index - correction) = xR(i, index - correction);
							break;
						}
						case 2:{
							xR2(r-1, index - correction) = xR(i, index - correction);
							break;
						}
						case 3:{
							xR3(g-1, index - correction) = xR(i, index - correction);
							break;
						}
						case 4:{
							xR4(h-1, index - correction) = xR(i, index - correction);
							break;
						}
						case 5:{
							xR5(j-1, index - correction) = xR(i, index - correction);
							break;
						}
						default: {break;}
					}
					
					
					if(endptr == val || errno != 0 || (*endptr != '\0' && !isspace(*endptr))){
						s << "Error in endptr ";
						::Rf_error(s.str().c_str());
						return R_NilValue;
					}
				}
				if(inst_max_index > max_index)
					max_index = inst_max_index;	
			}
				
			
			fclose(fp);
			free(line);
			//Rcpp::List rl = Rcpp::List::create (Rcpp::Named ("X", xR), Rcpp::Named("Y", yR) );
			Rcpp::List rl = Rcpp::List::create (Rcpp::Named ("X1", xR1), Rcpp::Named("Y1", yR1), Rcpp::Named ("X2", xR2), Rcpp::Named("Y2", yR2), Rcpp::Named ("X3", xR3), Rcpp::Named("Y3", yR3), Rcpp::Named ("X4", xR4), Rcpp::Named("Y4", yR4), Rcpp::Named ("X5", xR5), Rcpp::Named("Y5", yR5));
			return (rl);
		}
		
		
			
		else{
			//Fill NumericMatrix xR, NumericVector yR
			for ( i=0; i<l; i++)
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
						s << "Error in endptr ";
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
	}	
	
	catch(int e)
	{
		stringstream s;
		s << "Unknown Error: " << e << "\n";
		::Rf_error(s.str().c_str());
		return R_NilValue;
	}
} 



//' Write given (dense) R matrix and R vector in sparse (LIBSVM) format to given file.
//'
//' @param 	filename		the filename to write the given data to
//' @param		verbose		show verbose output?
//' @param		zeroBased	do the indices in the file start with 0, e.g. -1 0:2 1:4 ...?
//' @note		labels can any numeric, they are not converted in any way.
//' @keywords	IO 
//' @return		NULL.
//' @examples	
//'		X = as.matrix(iris[,1:4])
//'		Y = as.matrix(as.numeric(iris[,5]))
//'		writeSparseData ("./australian.data", X, Y)
//' @export
// [[Rcpp::export]] 
List writeSparseData (std::string filename, NumericMatrix X, NumericVector Y, bool verbose = false, bool zeroBased = false) {
	
	try
	{
		stringstream s;

		std::fstream fs;
		fs.open(filename.c_str(), std::fstream::in | std::fstream::out |std::fstream::trunc);
		
		int correction = 0;
		if (zeroBased == false) {
			correction = 1;
		}

		// determine dimensions;
		n = X.nrow();
		m = X.ncol();
		
		if (Y.size() != n) {
			Rcout << "Error!\n";
			// FIXME throw error
		}
		
		for(int i=0;i<n;i++) 
		{
			fs << Y(i) << " ";
			for(int j=0;j<m;j++)
			{
				if(X(i,j) != 0)
					fs << j + correction << ":" << std::setprecision(16) << X(i,j) << " ";
			}
			fs << "\n";
		}
		fs.close();
	}
	catch(int e)
	{
		stringstream s;
		s << "Unknown Error: " << e << "\n";
		::Rf_error(s.str().c_str());
	}
	return R_NilValue;
}

