#include "Population.h"
#include <iomanip>
#include <algorithm>

using std::remove;

Population::Population(int n, double a, double b, int size,
                       function<double(vector<double>)> fitness,
                       int tournament_t, int elitism_e) {
    p = new vector<Phenotype*>();
    this->size = size;
    p->resize(size);

    Phenotype::a = a;
    Phenotype::b = b;
    Phenotype::f = fitness;

    this->t = tournament_t;
    this->e = elitism_e;

    for (int i = 0; i < size; i++) {
        (*p)[i] = new Phenotype(n);
    }
}

Population::~Population() {
    for (int i = 0; i < size; i++) {
        delete (*p)[i];
    }
    delete p;
}

void Population::sort(vector<Phenotype*> *p) {
    bool sorted = true;
    for (int i = 0; i < size - 1; i++) {
        if ((*p)[i]->fitness() > (*p)[i+1]->fitness()) {
            sorted = false;
            break;
        }
    }
    if (sorted) return;

    for (int i = 0; i < size; i++) {
        Phenotype* best = (*p)[i]; 
        int best_i = i;
        
        for (int j = i + 1; j < size; j++) {
            if((*p)[j]->fitness() < best->fitness()) {
                best = (*p)[j]; best_i = j;
            }
        }
        
        (*p)[best_i] = (*p)[i];
        (*p)[i] = best;
    }
}

set<Phenotype*> * Population::elitism() {
    set<Phenotype*> *result = new set<Phenotype*> ();
    sort(this->p);
    
    for (int i = 0; i < this->e; i++) {
        result->insert((*p)[i]);
    }

    return result;
}

// assuming k < size of population
set<Phenotype*> * Population::take (int k) {
    set<Phenotype*> *result = new set<Phenotype*> ();
    int ind[k];

    for (int i = 0; i < k; i++) {
        int index = rand() % (k - i);
        for (int j = 0; j < i; j++) {
            if (index >= ind[j]) index++;
        }
        ind[i] = index;
    }

    for (int i = 0; i < k; i++) {
        result->insert((*p)[ind[i]]);
    }

    return result;
}

set<Phenotype*> * Population::tournament() {
    set<Phenotype*> *result = new set<Phenotype*> ();

    for (int i = 0; i < size; i++) {
        set<Phenotype*> *g = take(this->t);
        auto candidate = g->begin(); // *candidate is of type Phenotype*
        Phenotype* best = *candidate;
        while(++candidate != g->end()) {
            if ((*candidate)->fitness() < best->fitness()) {
                best = *candidate;
            }
        }
        result->insert(best);
        delete g;
    }

    return result;
}

// size of selected set is <= N
set<Phenotype*> * Population::select() {
    set<Phenotype*> *elite = elitism();
    set<Phenotype*> *champions = tournament(); 
    elite->insert(champions->begin(), champions->end());
    delete champions; 

    return elite;
}

vector<Phenotype*> * Population::reproduce(set<Phenotype*> *parents) {
    vector<Phenotype*> *children((parents->size()) * (parents->size()));
    int i = 0;
    for (auto mother : *parents) {
        for (auto father : *parents) {
            Phenotype* kid = new Phenotype(mother, father);
            children[i] = kid;
            i++;
        }
    }
    delete parents;

    return children;
}

void Population::clear() {
    for (auto parent : *p) {
        delete parent;
    }
    delete p;
}

void Population::evolve() {
    vector<Phenotype*> *children = reproduce(select());
  
    this->clear();
   
    for (auto kid : *children) {
        if (random_between(0,1) <= mutation_rate) kid->mutate();
    }
    p = children;

    set<Phenotype*> *selected = select();

    for (auto phen : *selected) {
        remove(p->begin(), p->end(), phen);
    }
    this->clear();

    p = new vector<Phenotype*>();
    p->assign(selected->begin(), selected->end());
}

void Population::show_population() {
    for (int i = 0; i < size; i++) {
        cout << i << ": "<< std::setprecision(2) << std::fixed;
        for (int j = 0; j < (*p)[i]->data().size(); j++) {
            cout << (*p)[i]->data()[j] << " ";
        }
        cout << "\tfitness = " << (*p)[i]->fitness() << endl;
    }
}

vector<double> Population::best_solution() {
    sort(this->p);
    return (*p)[0]->data();
}
