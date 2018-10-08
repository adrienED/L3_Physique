#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("ct.dat");


	
int main (void){
int n=10, i=0, j;
double x=0.0, p=1.0 , xx[n];
for(int j=0; j<=n;j++) 
xx[j]=((3.14/n)*j);
if (j!=i) p=p*((x-xx[j])/(xx[i]-xx[j]));
x=xx[j];
while(x<3.14){

fich<<x<<"       "<<p<<endl;
x=x+0.01;

}

fich.close();
return 0;
}


