#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("rk4.dat");


double F(double x,double f) {
	
	return f;
}


int main (void){
int n=4,a=0,b=1;
double x,f,k1,k2,k3,k4,i,h,xx[n],y,x0=0,f0=1;
x=x0;
f=f0;
for(int i=0;i<=n;i++){
xx[i]=a+h*i;
h=b-a/n;
}
x=xx[0];
while(x<b){
k1=F(x,f);
k2=F(x+0.5*h,f+0.5*h*k1);
k3=F(x+0.5*h,f+0.5*h*k2);
k4=F(x+h,f+h*k3);
f=f+(h/6.)*(k1+2*k2+2*k3+k4);
x=x+h;
fich<<x<<" "<<f<<endl;
}

fich.close();
return 0;

}

