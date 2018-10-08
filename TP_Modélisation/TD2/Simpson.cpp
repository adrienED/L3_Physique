#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("Simpson.dat");

double F(double x) {
return sin(x);
}
	
int W(int j, int n) {
int p;
if(j%2==0) p=2;
else p=4;
if (j==0) p=1;
if (j==n) p=1;
return p;
}		

int main (void){
int x;
double s=0; int n=200; 
double a=0.,b=M_PI/2,h=(b-a)/n;

for(int j=0;j<=n;j++){
	
x=a+h*j;
s=s+(h/3.)*W(j,n)*F(x);
}
	


cout<<s<<endl;

return 0;


}

