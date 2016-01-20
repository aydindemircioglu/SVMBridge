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
#include <sstream>
#include <fstream>
#include <iostream>
#include <iomanip>      // std::setprecision
#include <string>

#define Malloc(type,n) (type *)malloc((n)*sizeof(type))
#define DEBUG if (1 == 0) 

#ifdef __linux
	#define OS (const char*)("linux")
	#include <unistd.h>
	#include <sys/types.h>
	#include <pwd.h>
#elif __APPLE__
	#define OS (const char*)("mac")
	#include <unistd.h>
	#include <sys/types.h>
	#include <pwd.h>
#elif defined _WIN32 || defined _WIN64
	#define OS (const char*)("windows")
#endif

using namespace std;
using namespace Rcpp;

static char *line = NULL;
static int n=0;
static int m=0;
static int y_width=0;
static int max_line_len;


// TODOs: every X lines call Rcpp::checkUserInterrupt().


// internal helper function to read a line into global variable
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
List readSparseData (std::string filename, unsigned long  skipBytes = 0, bool verbose = false, bool zeroBased = false) {
	
	verbose = true;
	
	std::setprecision(16);
  
	try
	{	
		int correction = 1;
		if (zeroBased == true)
			correction = 0;
		
		int index = 0;
		int max_index = 0;
		int inst_max_index = 0;
		int min_index = 2;
		int l = 0;
		int featureDimension = 0;
		int alphacount = 0;
		int max_alphacount = 1;
		int test_for_multi = 0;
		
		string substring = "";
		string alphastring = "";
		string first_index = "";
		string os = "";
		
		char *endptr = NULL;
		char *idx = NULL;
		char *val = NULL;
		char *label = NULL;
		char *p = NULL;
		stringstream s;
		
		//tilde handling
		int tilde = filename.find("~");
		if(tilde != -1){
			
			#if defined _WIN32
				char* home_path;
				home_path = getenv("HOMEPATH");
				filename.erase(tilde,1);
				int slash = filename.find("\\");
				while(slash != -1){
						filename.replace(slash, 1, "//");
						slash = filename.find("\\");
				}
				filename = home_path + filename;
			#else
				// mac and linux works the same here
				char* home_path;
				home_path = getenv("HOME");
				if(home_path == NULL){
						struct passwd* pw = getpwuid(getuid());
						home_path = pw->pw_dir;
				}
				filename.erase(tilde,1);
				filename = home_path + filename;
			#endif
			if(verbose == TRUE){
				Rcout << "  Expanded path to dataset: " << filename << "\n";
			}
		}
		
		
		
		FILE *fp = fopen(filename.c_str(),"r");
		if(fp == NULL){
			s << "Can't open input file " << filename;
			::Rf_error(s.str().c_str());
			return R_NilValue;
		}
		
		//Jump to position in dataset where sparse data starts
		fseek(fp, skipBytes, SEEK_CUR);
		
		max_line_len = 1024;
		line = Malloc(char,max_line_len);
		
		//determine size
		while(readline(fp)!=NULL)  //global variable line updated
		{
			p = strtok(line," \t"); 
			inst_max_index = -1;
			double i = strtod(p,&endptr);
			
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
				
				//check if column contains multiple alpha values
				string idx3 = std::string(idx);
				
				test_for_multi = idx3.find(" ");
				
				if ((idx3.size() > 1) & (test_for_multi != -1)){
					int f = 0;
					//determine count of alpha values (check for unequal counts)
					for (unsigned int j=0; j < idx3.size(); j++){
						alphastring = std::string(&idx[j], 1);
						
						if(alphastring == " "){
							alphacount++;
							f=j+1;
						}
						first_index = idx3.substr(f, j);
					}
					
					if(alphacount > max_alphacount)
						max_alphacount = alphacount;
					
					//Might not detect unequal count of alpha values of prior columns but it saves time for unequality in subsequent columns
					if(alphacount < max_alphacount){
						s << "Error while loading file: Unequal count of alpha values detected " ;
						::Rf_error(s.str().c_str());
						return R_NilValue;
					}
					
					//fix idx so it doesnt contain any alpha values
					stringstream ss;
					ss << first_index;
					string tmp = ss.str();
					char idx2[1024];				
					strncpy(idx2, tmp.c_str(), sizeof(idx2));
					idx2[sizeof(idx2)-1] = 0;
					idx = idx2;
					alphacount = 0;
					//cout << "Fehlertracking: " << "idx = " << idx << endl;
				}
				
				errno = 0;
				index = (int) strtol(idx,&endptr,10); 
				
				if(index < min_index)
					min_index = index;
				
				//cout << "Fehlertracking: " << "\nindex = " << index << endl;
				
				if(endptr == idx || errno != 0 || *endptr != '\0' || index <= inst_max_index) {
					s << "Error while loading file. " ;
					::Rf_error(s.str().c_str());
					return R_NilValue;
				}
				else
					inst_max_index = index;
				
				errno = 0;
				double i = strtod(p,&endptr); 
				
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
		featureDimension = max_index + 1 - correction;

		n=l; //set global variable n for function writeSparseData
		m=max_index; //set global variable m for function writeSparseData
		//if dataset contains multiple alpha values, max_alphacount gets incremented since the first alpha value didnt get accounted yet FIXME
		if(max_alphacount > 1)
			max_alphacount++; 
			
		if (verbose == true) 
			Rcout << "\nCount of Alpha Values: " << max_alphacount << endl;
		
		if (verbose == true) 
			Rcout << "Found data dimensions: " << l << " x " << featureDimension << "\n";

		Rcpp::NumericMatrix xR(l,featureDimension);
		Rcpp::NumericMatrix yR(l,max_alphacount);
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
			
			//if datasets contains multiple alpha values, this is the first one, else this is the class label FIXME
			yR(i,0) = strtod(label, &endptr);
			//cout << "label " << label << endl;
			//cout << "endptr " << *endptr << endl;
			if(endptr == label || *endptr != '\0'){
					s << "Error in endptr " ;
					::Rf_error(s.str().c_str());
					return R_NilValue;
				}
			
			while(1)
			{
				std::setprecision(16);
				idx = strtok(NULL,":");
				val = strtok(NULL," \t");
				alphacount = 1; //correction since max_alphacount got incremented
				if(val == NULL) //No further informations in this line.. end of line
					break;
				
				//check if column contains multiple alpha values
				string idx3 = std::string(idx);
				test_for_multi = idx3.find(" ");
				int f = 0;
				if ((idx3.size() > 1) & (test_for_multi != -1)) {
					int k = 1;
					
					//fill yR with alpha values
					for (unsigned int j=0;j<idx3.size();j++){
						
						alphastring = std::string(&idx[j], 1);
						if(alphastring == " "){
							alphacount++;
							substring = idx3.substr(f, j-f);
							double alphavalue = strtod(substring.c_str(), NULL);
							yR(i, k) = alphavalue;  
							f = j+1;
							k++;
						}
						first_index = idx3.substr(f, j);
						//DEBUG Rcout << "first_index = " << first_index << endl;
						//cout << "first_index = " << first_index << endl;
					}
					
					//final check for unequality in alpha values count. 
					if(alphacount < max_alphacount){
						s << "Error while loading file: Unequal count of alpha values detected " ;
						::Rf_error(s.str().c_str());
						return R_NilValue;
					}
					
					//fix idx so it doesnt contain any alpha values
					
					stringstream ss;
					ss << first_index;
					string tmp = ss.str();
					char idx2[1024];				
					strncpy(idx2, tmp.c_str(), sizeof(idx2));
					idx2[sizeof(idx2)-1] = 0;
					idx = idx2;
					//cout << "fixed idx = " << idx << endl;
				}
				
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
				//cout << "Fehlertracking val2 = " << rds << endl;
				xR(i, index - correction) = strtod(val,&endptr); //errno may be updated in this step
				//cout  << "Fehlertracking: index = " << index << endl;
				
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
List writeSparseData (std::string filename, NumericMatrix X, NumericMatrix Y, bool append = false,
	unsigned long  skipBytes = 0, bool verbose = false, bool zeroBased = false) {
	
	try
	{
		stringstream s;

		std::fstream fs;
		if (append == true) {
			fs.open(filename.c_str(), std::fstream::in | std::fstream::out |std::fstream::app);
//			fs.seekg(skipBytes);
		} else {
			fs.open (filename.c_str(), std::fstream::in | std::fstream::out | std::fstream::trunc);
		}
		
		int correction = 0;
		if (zeroBased == false) {
			correction = 1;
		}

		// determine dimensions;
		n = X.nrow();
		m = X.ncol();
		y_width = Y.ncol();
		
		
		if (Y.nrow() != n) {
			Rcout << "Error!\n";
			// FIXME throw error
		}
		
		for(int i=0;i<n;i++) 
		{
			for(int r=0;r<y_width;r++){
				fs << std::setprecision(16) << Y(i,r) << " ";
			}
			
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

