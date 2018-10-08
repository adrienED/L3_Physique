#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("2integrale.dat");


double F(double x, int a, double f[]) {
	double p;
	if(a==0) p=f[0]+2-pow(0,2);
	if(a==1) p=f[1]+2-pow(1,2);
	if(a==2) p=f[2]+2-pow(2,2);
    if(a==3) p=f[3]+2-pow(3,2);

	return p;
}

int main (void){
int n=1;
double x,f[n],k1[n],k2[n],k3[n],k4[n],h,xn,a,b,e;
e=exp(1);
a=0;
b=1;
x=0;
f[0]=1; 
h=0.001;

double kk[n]; //variable suplémentaire

while(x<xn){
	
for(int a=0;a<n;a++){
	k1[a]=F(x,a,f); kk[a]=f[a]+0.5*h*k1[a];
}		
	
for(int a=0;a<n;a++){
	k2[a]=F(x+0.5*h,a,kk); kk[a]=f[a]+0.5*h*k2[a];
}	
		
for(int a=0;a<n;a++){
	k3[a]=F(x+0.5*h,a,kk); kk[a]=f[a]+h*k3[a];		
	
}	
for(int a=0;a<n;a++){
	k4[a]=F(x+h,a,kk);
}	


for(int a=0;a<n;a++){
	f[a]=f[a]+(h/6.)*(k1[a]+2.*k2[a]+2.*k3[a]+k4[a]); 
}
x=x+h;
fich<<x<<" "<<f[0]<<" "<<endl;

cout<<f[0]<<endl;

}

fich.close();
return 0;

}
