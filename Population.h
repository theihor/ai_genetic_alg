#include "Phenotype.h"
#include <set>

using std::set;

class Population {
    public:
        Population(int n, double a, double b, int size,
                function<double(vector<double>)> fitness,
                int tournament_t, int elitism_e);
        ~Population();

        void clear(); // clears p


        void show_population();

        void evolve(); // the evolution step
        
        vector<double> best_solution();

        const double mutation_rate = 0.05; // probability to mutate for one phenotype
        
    private:
        vector<Phenotype*> *p; // phenotypes
        int size;

        int t; // for tournament
        int e; // for elitism (count of unchanged phenotypes)

        void sort(vector<Phenotype*> *p); // sorts p by fitness

        set<Phenotype*> * take(int k); // randomly chooses k different phenotypes from p

        set<Phenotype*> * elitism();
        set<Phenotype*> * tournament();
        set<Phenotype*> * select(); // the selection stage

        vector<Phenotype*> * reproduce(set<Phenotype*> *parents);

};
