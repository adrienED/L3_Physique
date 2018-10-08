#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("BETA");

double Phi(double x, int n, int i, double xx[]) {
	double p=1.0;
	for(int j=0; j<=n;j++)
	if (j!=i) p=p*((x-xx[j])/(xx[i]-xx[j]));

return p;
}
	
int main (void){
int n=5, i=0,j;
double x=0.0, p=1.0 , xx[n+1], g;


for(int j=0; j<=n;j++) {
xx[j]=((3.14/n)*j);
cout << xx[j]<<endl;
}

x=xx[0];

cout<<Phi(0.1,n,1,xx)<<endl;

while(x<=xx[n]){

g=0.;

for(int j=0;j<=n;j++)
g=g+(sin(xx[j])*Phi(x,n,j,xx));	


fich<<x<<"     "<<g<<endl;
x=x+0.01;
}


fich.close();
return 0;


}

