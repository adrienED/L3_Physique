#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("sin.dat");
int main()
{
    float I = 0;
    while(I < M_PI) {
        printf("Sin(%f)=%f\n",I,sin(I));
        I += 0.1;
        
        fich<<I<<"   "<<sin(I)<<endl;
	}
        fich.close();
    
    return 0;
}



