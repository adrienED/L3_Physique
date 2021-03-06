#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("c1.dat");

double Phi(double x, int n, int i, double xx[]) {
	double p=1.0;
	for(int j=0; j<=n;j++)
	if (j!=i) p=p*(x-xx[j])/(xx[i]-xx[j]);

return p;
}
	
int main (void){
int n=3, i=0,j;
double x=1.1, p=1.0 , xx[n];
xx[0]=0.0;
xx[1]=0.5;
xx[2]=1.0;
xx[3]=2.0;

for (j=0; j<=n; j++) {
if (j!=i) p=p*(x-xx[j])/(xx[i]-xx[j]);
}
cout<<" x="<<x<<" i="<<i<<" p="<<p<<endl;

cout<<"Phi="<<Phi(x,n,i,xx)<<endl;


x=xx[0];
while(x<xx[n]){
fich<<x<<" "<<Phi(x,n,i,xx)<<endl;
x=x+0.01;
}

fich.close();
return 0;

}

