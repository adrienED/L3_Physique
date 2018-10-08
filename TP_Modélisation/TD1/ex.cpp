#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("ex.dat");


	
int main (void){
int n=10, i=0;
double x=-1.0;

while(x<1){

fich<<x<<"       "<<exp(x)<<endl;
x=x+0.01;

}

fich.close();
return 0;
}


