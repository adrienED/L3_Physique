#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("c3.dat");

double Phi(double x, int n, int i, double xx[]) {
	double p=1.0;
	for(int j=0; j<=n;j++)
	xx[j]=(3.14/n)*j;
	if (j!=i) p=p*((x-xx[i])/(xx[j]-xx[j]));

return p;
}
	
int main (void){
int n=1, i=0,j=0, g;
double x=1.0, p=1.0 , xx[n], xx[g]=0;
xx[0]=0;


while(xx[n]<=3.14){
double xx[g]=double sin (double xx[j])*p
fich<<x<<" "<<xx[g]<<endl;
x=x+0.01;
}

fich.close();
return 0;

}


