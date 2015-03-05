#include <stdlib.h>
#include <vector>
#include <time.h>
#include <functional>
#include <memory>
#include <iostream>

using std::cout;
using std::endl;
using std::function;
using std::vector;
using std::shared_ptr;

typedef struct {
    int index;
    int bit;
} crossover_point;

double random_between(double a, double b);

class Phenotype {
    private:
        vector<double> ch; // chromosomes
        double fitness_value;

        crossover_point random_point(int max_index, int max_bit);
        double crossover_double(double p1, double p2, int bit);

        static int seed;
        const double mutation_rate = 0.2; // affects depth of mutation 
    public:
        static double a; // lower bound of Xi
        static double b; // upper bound of Xi
        static function<double(vector<double>)> f;

        Phenotype(vector<double> ch, double fitness_value); // basic constructor
        Phenotype(int n); // random phenotype
        Phenotype(Phenotype *p1, Phenotype *p2); // crossover is going on here

        double fitness() { return fitness_value; }
        vector<double> data() { return ch; }

        void mutate();
};
