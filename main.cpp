#include "Population.h"

const int n = 5;
const double a = -5;
const double b = 5;
const int population_size = 10;

const int evolution_steps = 10;

const int t = 2;
const int e = population_size / 10;

double hypersphere(vector<double> v) {
    double result = 0;
    for (int i = 0; i < v.size(); i++) {
        result += v[i] * v[i];
    }

    return result;
}

int main() {
    Population p(n, a, b, population_size, hypersphere, t, e);
    
    for (int i = 0; i < evolution_steps; i++) {
        vector<double> solution = p.best_solution();
        cout << "Solution: ";
        for (int j = 0; j < solution.size(); j++)
            cout << solution[j] << " ";
        cout << endl << "fitness = " << hypersphere(solution) << endl;

        p.evolve();
    }
    
    return 0;
}
