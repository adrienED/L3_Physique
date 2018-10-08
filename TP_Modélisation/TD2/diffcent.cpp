#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("diffcent.dat");

double F1(double x, double h) {	
return exp(x+h);
}

double F2(double x, double h) {	
return exp(x-h);
}

int main (void){
double t=-15;
double D=0; int n=1000; 
double a=0,b=-8,h=(b-a)/n,E,x;


while(t<=-8){
h=exp(t);	
x=0;
D=(F1(x,h)-F2(x,h))/(2*h);
E=abs(D-exp(x));

fich<<t<<"     "<<log(E)<<endl;
t=t+0.001;
}

fich.close();


return 0;


}


