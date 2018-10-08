#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("run.dat");

double M(double x, int n, int i, double xx[]) {
	double p=1.0;
	for(int j=0; j<=n;j++)
	if (j!=i) p=p*((x-xx[j])/(xx[i]-xx[j]));

return p;
}
	
int main (void){
int n=14;
double f(n), x[n], s1,s2,s3,s4,x(0)=,f(0)=;

for(int j=0; j<=n;j++) {
xx[j]=(((1./2.)*((-1.)+(1.)-(1.+1.))*(cos((M_PI*(2.*j+1.)/(2.*n+1.))/(cos(M_PI/(2.*(n+1.)))); 
}

s1=0;
for(int i=0; i<n;++) {
s1=s1+x[j];
}

s2=0;
for(int i=0; i<n;++) {
s2=s1+x[j];
}


s3=0;
for(int i=0; i<n;++) {
s3=s1+x[j];
}


s4=0;
for(int i=0; i<n;++) {
s4=s1+x[j];
}


U1=0;
for(int i=0; i<n;++) {
U1=s1+x[j];
}





for(int j=0; j<=n;j++) {
}

x=xx[0];


while(x<=1){

g=0.;

for(int j=0;j<=n;j++)
g=g+((1/(1+25*pow(x,2))*Phi(x,n,j,xx)));	


fich<<x<<"     "<<g<<endl;
x=x+0.01;
}


fich.close();
return 0;


}
