#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
	
int main (void){
double Phi(double x, int n, int i, double xx[]) {
	double p=1.0;
	for(int j=0; j<=n;j++)
	if (j!=i) p=p*x-xx[j]/(xx[i]-xx[j]);

return p;
}

