#include <math.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "svm.h"

#define Malloc(type,n) (type *)malloc((n)*sizeof(type))

// yes, this was not the best idea..


typedef signed char schar;
typedef float Qfloat;
#ifndef min
template <class T> static inline T min(T x,T y) { return (x<y)?x:y; }
#endif
#ifndef max
template <class T> static inline T max(T x,T y) { return (x>y)?x:y; }
#endif
template <class T> static inline void swap(T& x, T& y) { T t=x; x=y; y=t; }
template <class S, class T> static inline void clone(T*& dst, S* src, int n)
{
    dst = new T[n];
    memcpy((void *)dst,(void *)src,sizeof(T)*n);
}
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
#define INF HUGE_VAL
#define TAU 1e-12

//
// Kernel Cache
//
// l is the number of total data items
// size is the cache size limit in bytes
//
class Cache
{
public:
    Cache(int l,long int size);
    ~Cache();

    // request data [0,len)
    // return some position p where [p,len) need to be filled
    // (p >= len if nothing needs to be filled)
    int get_data(const int index, Qfloat **data, int len);
    void swap_index(int i, int j);  
private:
    int l;
    long int size;
    struct head_t
    {
        head_t *prev, *next;    // a circular list
        Qfloat *data;
        int len;        // data[0,len) is cached in this entry
    };

    head_t *head;
    head_t lru_head;
    void lru_delete(head_t *h);
    void lru_insert(head_t *h);
};


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

    static double k_function(const svm_node *x, const svm_node *y,
                 const svm_parameter& param);
    virtual Qfloat *get_Q(int column, int len) const = 0;
    virtual double *get_QD() const = 0;
    virtual void swap_index(int i, int j) const // no so const...
    {
        swap(x[i],x[j]);
        if(x_square) swap(x_square[i],x_square[j]);
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





int print_null(const char *s,...) {return 0;}

static int (*info)(const char *fmt,...) = &printf;

struct svm_node *x;
int max_nr_attr = 64;

struct svm_model* model;
int predict_probability=0;

static char *line = NULL;
static int max_line_len;

struct svm_problem prob;        // set by read_problem
struct svm_node *x_space;
//struct svm_parameter param;     // set by parse_command_line
const char* trainfile = NULL;


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

void exit_input_error(int line_num)
{
	fprintf(stderr,"Wrong input format at line %d\n", line_num);
	exit(1);
}

void predict(FILE *input, FILE *output)
{
	int correct = 0;
	int total = 0;
	double error = 0;
	double sump = 0, sumt = 0, sumpp = 0, sumtt = 0, sumpt = 0;

	int svm_type=svm_get_svm_type(model);
	int nr_class=svm_get_nr_class(model);
	double *prob_estimates=NULL;
	int j;

	if(predict_probability)
	{
		if (svm_type==NU_SVR || svm_type==EPSILON_SVR)
			info("Prob. model for test data: target value = predicted value + z,\nz: Laplace distribution e^(-|z|/sigma)/(2sigma),sigma=%g\n",svm_get_svr_probability(model));
		else
		{
			int *labels=(int *) malloc(nr_class*sizeof(int));
			svm_get_labels(model,labels);
			prob_estimates = (double *) malloc(nr_class*sizeof(double));
			fprintf(output,"labels");		
			for(j=0;j<nr_class;j++)
				fprintf(output," %d",labels[j]);
			fprintf(output,"\n");
			free(labels);
		}
	}

	
    
    // --- extra computations
    if (trainfile != NULL)
    {
        printf("Computing extra values..\n");
        // some stupid things to initialize..
        int l = model->l;
        schar *y = new schar[l];
        int i;

        printf("Having %d SVs\n", l);
        
        for(i=0;i<l;i++)
        {
            y[i] = -1;
            if (model->sv_coef[0][i] > 0)
                y[i] = +1;
        }

        printf("Obtained cost C: %f\n", model->param.C);
        printf("Obtained gamma g: %f\n", model->param.gamma);
        double Cp = model->param.C;
        
        // compute hinge loss
        
        printf("Current rho: [%f]\n", model->rho[0]);
        
        double hingeLoss = 0.0;
        for (int i = 0; i < prob.l; i++)
        {
            // predict value
            {
                double currentLoss = 0.0;
                svm_predict_values(model, prob.x[i], &currentLoss);
                currentLoss = 1.0 - double(prob.y[i]) * currentLoss; 
                if (currentLoss > 0)
                    hingeLoss += currentLoss;
            }
        }
        printf("Current hingeLoss: [%f]\n", hingeLoss);
    

        // first term
        double primal = Cp * hingeLoss;
        
        // compute weight squared
        double weight = 0;
        for(int i=0;i<l;i++)
        {
            for(int j=i;j<l;j++)
            {
                double k = Kernel::k_function(model->SV[i], model->SV[j], model->param);
                k = model->sv_coef[0][i] * k * model->sv_coef[0][j];
                weight += k;
                if (j != i)
                    weight += k;
            }
        }
        weight = weight/2.0;
        printf("Current weight: [%f]\n", sqrt(2*weight));

        // second term
        primal += weight;
        
        // compute dual
        double dual = -weight;
        
        for (int j=0; j<l; j++)
        {
            dual += model->sv_coef[0][j] * y[j];
        }

        
        // do not count computation of primal to the save times
        printf("Computed primal value: [%f]\n", primal);
        printf("Computed dual value: [%f]\n", dual);
        fflush(stdout);
        
        // FREE ALPHAS, minus_ones , y
    } 
    
    // ---
    
    
	max_line_len = 1024;
	line = (char *)malloc(max_line_len*sizeof(char));
	while(readline(input) != NULL)
	{
		int i = 0;
		double target_label, predict_label;
		char *idx, *val, *label, *endptr;
		int inst_max_index = -1; // strtol gives 0 if wrong format, and precomputed kernel has <index> start from 0

		label = strtok(line," \t\n");
		if(label == NULL) // empty line
			exit_input_error(total+1);

		target_label = strtod(label,&endptr);
		if(endptr == label || *endptr != '\0')
			exit_input_error(total+1);

		while(1)
		{
			if(i>=max_nr_attr-1)	// need one more for index = -1
			{
				max_nr_attr *= 2;
				x = (struct svm_node *) realloc(x,max_nr_attr*sizeof(struct svm_node));
			}

			idx = strtok(NULL,":");
			val = strtok(NULL," \t");

			if(val == NULL)
				break;
			errno = 0;
			x[i].index = (int) strtol(idx,&endptr,10);
			if(endptr == idx || errno != 0 || *endptr != '\0' || x[i].index <= inst_max_index)
				exit_input_error(total+1);
			else
				inst_max_index = x[i].index;

			errno = 0;
			x[i].value = strtod(val,&endptr);
			if(endptr == val || errno != 0 || (*endptr != '\0' && !isspace(*endptr)))
				exit_input_error(total+1);

			++i;
		}
		x[i].index = -1;

        
		if (predict_probability && (svm_type==C_SVC || svm_type==NU_SVC))
		{
			predict_label = svm_predict_probability(model,x,prob_estimates);
			fprintf(output,"%g",predict_label);
			for(j=0;j<nr_class;j++)
				fprintf(output," %g",prob_estimates[j]);
			fprintf(output,"\n");
		}
		else
		{
			predict_label = svm_predict(model,x);
			fprintf(output,"%g\n",predict_label);
		}

		if(predict_label == target_label)
			++correct;
		error += (predict_label-target_label)*(predict_label-target_label);
		sump += predict_label;
		sumt += target_label;
		sumpp += predict_label*predict_label;
		sumtt += target_label*target_label;
		sumpt += predict_label*target_label;
		++total;
	}
	if (svm_type==NU_SVR || svm_type==EPSILON_SVR)
	{
		info("Mean squared error = %g (regression)\n",error/total);
		info("Squared correlation coefficient = %g (regression)\n",
			((total*sumpt-sump*sumt)*(total*sumpt-sump*sumt))/
			((total*sumpp-sump*sump)*(total*sumtt-sumt*sumt))
			);
	}
	else
		info("Accuracy = %g%% (%d/%d) (classification)\n",
			(double)correct/total*100,correct,total);
	if(predict_probability)
		free(prob_estimates);
    
}

void exit_with_help()
{
	printf(
	"Usage: svm-predict [options] test_file model_file output_file\n"
	"options:\n"
    "-o train_file : if specified some derivates like primal value, dual value etc will be computed\n"
	"-b probability_estimates: whether to predict probability estimates, 0 or 1 (default 0); for one-class SVM only 0 is supported\n"
	"-q : quiet mode (no outputs)\n"
	);
	exit(1);
}


// read in a problem (in svmlight format)

void read_problem(const char *filename)
{
    int elements, max_index, inst_max_index, i, j;
    FILE *fp = fopen(filename,"r");
    char *endptr;
    char *idx, *val, *label;

    if(fp == NULL)
    {
        fprintf(stderr,"can't open input file %s\n",filename);
        exit(1);
    }

    prob.l = 0;
    elements = 0;

    max_line_len = 1024;
    line = Malloc(char,max_line_len);
    while(readline(fp)!=NULL)
    {
        char *p = strtok(line," \t"); // label

        // features
        while(1)
        {
            p = strtok(NULL," \t");
            if(p == NULL || *p == '\n') // check '\n' as ' ' may be after the last feature
                break;
            ++elements;
        }
        ++elements;
        ++prob.l;
    }
    rewind(fp);

    if (prob.l == 0) 
    {
        fprintf (stderr, "Trainfile has no data?!\n");
        exit_input_error(-17);
    }
    
    prob.y = Malloc(double,prob.l);
    prob.x = Malloc(struct svm_node *,prob.l);
    x_space = Malloc(struct svm_node,elements);

    max_index = 0;
    j=0;
    for(i=0;i<prob.l;i++)
    {
        inst_max_index = -1; // strtol gives 0 if wrong format, and precomputed kernel has <index> start from 0
        readline(fp);
        prob.x[i] = &x_space[j];
        label = strtok(line," \t\n");
        if(label == NULL) // empty line
            exit_input_error(i+1);

        prob.y[i] = strtod(label,&endptr);
        if(endptr == label || *endptr != '\0')
            exit_input_error(i+1);

        while(1)
        {
            idx = strtok(NULL,":");
            val = strtok(NULL," \t");

            if(val == NULL)
                break;

            errno = 0;
            x_space[j].index = (int) strtol(idx,&endptr,10);
            if(endptr == idx || errno != 0 || *endptr != '\0' || x_space[j].index <= inst_max_index)
                exit_input_error(i+1);
            else
                inst_max_index = x_space[j].index;

            errno = 0;
            x_space[j].value = strtod(val,&endptr);
            if(endptr == val || errno != 0 || (*endptr != '\0' && !isspace(*endptr)))
                exit_input_error(i+1);

            ++j;
        }

        if(inst_max_index > max_index)
            max_index = inst_max_index;
        x_space[j++].index = -1;
    }

    if(model->param.gamma == 0 && max_index > 0)
    {
        model->param.gamma = 1.0/max_index;
    }

    if(model->param.kernel_type == PRECOMPUTED)
        for(i=0;i<prob.l;i++)
        {
            if (prob.x[i][0].index != 0)
            {
                fprintf(stderr,"Wrong input format: first column must be 0:sample_serial_number\n");
                exit(1);
            }
            if ((int)prob.x[i][0].value <= 0 || (int)prob.x[i][0].value > max_index)
            {
                fprintf(stderr,"Wrong input format: sample_serial_number out of range\n");
                exit(1);
            }
        }

    fclose(fp);
}



int main(int argc, char **argv)
{
	FILE *input, *output;
	int i;
    
    double Cparam = -1.41;
    
	// parse options
	for(i=1;i<argc;i++)
	{
		if(argv[i][0] != '-') break;
		++i;
		switch(argv[i-1][1])
		{
            case 'o':
                trainfile = argv[i];
                break;
            case 'c':
                Cparam = atof(argv[i]);
                break;
			case 'b':
				predict_probability = atoi(argv[i]);
				break;
			case 'q':
				info = &print_null;
				i--;
				break;
			default:
				fprintf(stderr,"Unknown option: -%c\n", argv[i-1][1]);
				exit_with_help();
		}
	}

	if ((trainfile != NULL) && (Cparam == -1.41)) 
    {
        fprintf(stderr, "Need to specify the cost, if trainfile option is used.");
        exit_with_help();
    }
	
	if(i>=argc-2)
		exit_with_help();

	input = fopen(argv[i],"r");
	if(input == NULL)
	{
		fprintf(stderr,"can't open input file %s\n",argv[i]);
		exit(1);
	}

	output = fopen(argv[i+2],"w");
	if(output == NULL)
	{
		fprintf(stderr,"can't open output file %s\n",argv[i+2]);
		exit(1);
	}

	if((model=svm_load_model(argv[i+1]))==0)
	{
		fprintf(stderr,"can't open model file %s\n",argv[i+1]);
		exit(1);
	}

	model->param.C = Cparam;
    
	// if we specified a train file, we need to read it
	if (trainfile != NULL)
    {
        printf("Reading training file..\n");
        read_problem(trainfile);
    }
    
	
	x = (struct svm_node *) malloc(max_nr_attr*sizeof(struct svm_node));
	if(predict_probability)
	{
		if(svm_check_probability_model(model)==0)
		{
			fprintf(stderr,"Model does not support probabiliy estimates\n");
			exit(1);
		}
	}
	else
	{
		if(svm_check_probability_model(model)!=0)
			info("Model supports probability estimates, but disabled in prediction.\n");
	}

	printf("Predicting..\n");
	predict(input,output);
	svm_free_and_destroy_model(&model);
	free(x);
	free(line);
	fclose(input);
	fclose(output);
	return 0;
}
