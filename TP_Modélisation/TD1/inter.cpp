#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("ci.dat");

double Phi(double x, int n, int i, double xx[]) {
	double p=1.0;
	for(int j=0; j<=n;j++)
	if (j!=i) p=p*((x-xx[j])/(xx[i]-xx[j]));

return p;
}


int main (void){
int n=5, i=0,j;
double x= 0.0, p=1.0 , xx[n];
for (int j =0; j<=n; j++)
xx[j]=((M_PI/n)*j);

while(x<M_PI){
fich<<x<<" "<<(Phi(x,n,i,xx)*sin(xx[j]))<<endl;
x=x+0.01;
}

fich.close();
return 0;

}


