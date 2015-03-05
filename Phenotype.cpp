#include "Phenotype.h"

double Phenotype::a = 0;
double Phenotype::b = 0;
function<double(vector<double>)> Phenotype::f = [](vector<double> v){ return 0; };

Phenotype::Phenotype(vector<double> chromosomes, double fitness_value) {
    this->ch = chromosomes;
    this->fitness_value = fitness_value;
}

int Phenotype::seed = 0;

double random_between(double a, double b) {
    return a + ((double) rand() / RAND_MAX) * (b - a);
}

Phenotype::Phenotype(int n) {
    ch = vector<double>();
    ch.resize(n);
    srand( time(NULL) ^ seed++ );

    for (int i = 0; i < n; i++) {
        ch[i] = random_between(a, b);
    }

    this->fitness_value = f(ch);
}

crossover_point Phenotype::random_point(int max_index, int max_bit) {
    crossover_point p;
    p.index = rand() % max_index;
    p.bit = rand() % max_bit;

    return p;
}

// returns char with n lower bits as 1
// bitmask(4) --> 00001111b
char bitmask(int n) {
    char result = 0;
    for (int i = 0; i < n % 9; i++) {
        result <<= 1;
        result += 1;
    }
    return result;
}

double Phenotype::crossover_double(double p1, double p2, int bit) {
    char* c1 = (char *) &p1;
    char* c2 = (char *) &p2;
    double result = 0;
    char* r = (char *) &result;

    int i = 0;
    while (i + 8 < bit) {
        *r = *c1;
        r++; c1++; c2++;
        i += 8;
    }
    char mask2 = bitmask(8 - (bit - i));
    char mask1 = 0xFF ^ mask2;
    *r = (*c1) & mask1 | (*c2) & mask2; 
    r++; c1++; c2++; i += 8;

    while (i + 8 <= sizeof(double) * 8) {
        *r = *c2;
        r++; c1++; c2++;
        i += 8;
    }

    return result;
}

Phenotype::Phenotype(Phenotype *p1, Phenotype *p2) {
    if (p1 == p2) { 
        // cloning
        this->ch = p1->ch;
        this->fitness_value = p1->fitness_value;
    } else {
        // crossover
        crossover_point cp1 = this->random_point(p1->ch.size(), sizeof(double) * 8 + 1);
        crossover_point cp2 = this->random_point(p2->ch.size(), sizeof(double) * 8 + 1);
    
        // making cp1 < cp2
        if (cp1.index > cp2.index || cp1.index == cp2.index && cp1.bit > cp2.bit) {
            crossover_point temp = cp1;
            cp1 = cp2;
            cp2 = temp;
        }

        this->ch.resize(p1->ch.size());

        // 0 -> cp1
        int i = 0;
        while (i < cp1.index) {
            ch[i] = p1->ch[i];
            i++;
        }
        ch[i] = crossover_double(p1->ch[i], p2->ch[i], cp1.bit);
        // cp1 -> cp2
        while (i < cp2.index) {
            ch[i] = p2->ch[i];
            i++;
        }
        ch[i] = crossover_double(p2->ch[i], p1->ch[i], cp2.bit);
        // cp2 -> end
        while (i < ch.size()) {
            ch[i] = p1->ch[i];
            i++;
        }

        this->fitness_value = f(ch);
    }
}

void Phenotype::mutate() {
    int index = rand() % ch.size();
    double mutation = random_between(-0.5, 0.5) * mutation_rate * (b - a);
    ch[index] += mutation;
    if (ch[index] > b) ch[index] = b;
    else if (ch[index] < a) ch[index] = a;

    this->fitness_value = f(ch);
}
