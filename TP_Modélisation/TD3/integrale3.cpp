#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("integrale3.dat");


double F(double x, int a, double f[]) {
	double p;	
	
		if(a==0) p=f[1];
	if(a==1) p=-1./x*f[1]-(1.-0.25/x/x)*f[0];
	
	return p;
}

int main (void){
int n=3;
double x,f[n],k1[n],k2[n],k3[n],k4[n],h,a,b;

a=0.01;
b=10.;
x=0.01;
f[0]=0.1; 
f[1]=5.;
h=0.001;

double kk[n]; //variable suplémentaire

while(x<=b){
	
for(int a=0;a<n;a++){
	k1[a]=F(x,a,f); kk[a]=f[a]+0.5*h*k1[a];
}		
	
for(int a=0;a<n;a++){
	k2[a]=F(x+0.5*h,a,kk); kk[a]=f[a]+0.5*h*k2[a];
}	
		
for(int a=0;a<=n;a++){
	k3[a]=F(x+0.5*h,a,kk); kk[a]=f[a]+h*k3[a];		
	
}	
for(int a=0;a<n;a++){
	k4[a]=F(x+h,a,kk);
}	


for(int a=0;a<n;a++){
	f[a]=f[a]+(h/6.)*(k1[a]+2.*k2[a]+2.*k3[a]+k4[a]); 
}
x=x+h;

fich<<x<<" "<<f[0]<<endl;
cout<<x<<" "<<f[0]<<endl;
}

if(x=3) cout<<x<<" "<<f[0]<<" "<<endl;
fich.close();
return 0;

}
