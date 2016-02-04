CXX ?= g++
CFLAGS = -Wall -Wconversion -O3 -fPIC
SHVER = 2

all: svm-train svm-predict 

svm-predict: svm-predict.cpp cvm.o utility.o sgraph.o svm.o   cvm_core.o bvm.o
	$(CXX) $(CFLAGS) svm-predict.cpp  cvm.o utility.o sgraph.o svm.o  cvm_core.o bvm.o  -o svm-predict -lm
svm-train: svm-train.cpp cvm.o utility.o sgraph.o svm.o   cvm_core.o bvm.o
	$(CXX) $(CFLAGS) svm-train.cpp cvm.o utility.o sgraph.o svm.o  cvm_core.o bvm.o -o svm-train -lm

%.o : %.cpp
	$(CC) $(CFLAGS) -c $<
	
clean:
	rm -f *~ *.o  svm-train svm-predict 
