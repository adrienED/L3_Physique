#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("c3.dat");

double Phi(double x, int n, int i, double xx[]) {
	double p=1.0;
	for(int j=0; j<=n;j++)
	if (j!=i) p=p*((x-xx[j])/(xx[i]-xx[j]));

return p;
}
	
int main (void){
int n=4, i=0,j;
double x=1.0, p=1.0 , xx[n];
xx[0]=-1;
xx[1]=-0.7;
xx[2]=0;
xx[3]=0.2;
xx[4]=1;



x=xx[0];
while(x<xx[n]){
fich<<x<<" "<<Phi(x,n,i,xx)<<endl;
x=x+0.01;
}

fich.close();
return 0;

}
