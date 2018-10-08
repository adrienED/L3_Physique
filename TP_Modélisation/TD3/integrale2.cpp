#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("integrale2.dat");


double F(double x, int a, double f[]) {
	double p;
	if(a==0) p=f[0]+2-pow(x,2);
	return p;
}

int main (void){
int n=2;
double x,f[n],k1[n],k2[n],k3[n],k4[n],h,xn,a,b;

a=0;
b=3;
x=0;
xn=10;
f[0]=-1; 
h=(b-a)/100;

double kk[n]; //variable suplémentaire

while(x<=b){
	
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


fich<<x<<" "<<f[0]<<" "<<f[1]<<" "<<endl;
}
cout<<"f"<<" "<<f[0]<<" "<<endl;
if(x=3) cout<<x<<" "<<f[0]<<" "<<endl;
fich.close();
return 0;

}


