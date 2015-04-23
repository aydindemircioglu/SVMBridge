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


using namespace std;
using namespace Rcpp;


//' @export
// [[Rcpp::export]] 
RcppExport SEXP readSparseData (SEXP filename, SEXP verbose = false) {
	
	return R_NilValue;
}



//' @export
// [[Rcpp::export]] 
RcppExport SEXP writeSparseData (SEXP x, SEXP y, SEXP filename, SEXP verbose = false) {
	
	return R_NilValue;
}

