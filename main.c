/* 
 * File:   main.c
 * Author: bozhin
 *
 * Created on 25.07.2012, 12:37
 */

#include <stdio.h>
#include <stdlib.h>
#include "sc.h"



double es[N];
double eFermi[N];


double temp=90.0/11604.0;
double ff=0.34;

int main(int argc, char** argv)
{
    setriang_();
    int n = N;
    int i;
/*
    int ifprint=1;
    //Calculation of Es and Ef
    getesef_(&n, sParam, &temp, e, t, es, eFermi,
            &fillingFactor, &ifprint);
    
    printf("double esParam[] = {");
    for (i=0;i<N;i++) printf("%7.3lf,",es[i]);
    printf("};\n");
    printf("double efParam[] = {");
    for (i=0;i<N;i++) printf("%7.3lf,",eFermi[i]);
    printf("};\n");
*/
/*

    // Calculation of van Hove energy
    double vhe[4];
    printf("double vhParam[] = {");
    for (i=0; i<N; i++)
    {
        e[1]=esParam[i];
        vhenergy_(e,t,vhe);
        printf("%7.3lf,",vhe[2]);
    }
    printf("};\n");
*/
/*

    // Constant energy curve
    e[1]=esParam[12];
    cec_(&efParam[12],e,t);
*/
/*
    // Surfaces
    e[1]=esParam[12];
    surf_(e,t);
*/
/*
    //Density of states DOS
    scdos_();
*/
/*
    // Tc-clone algorithm
    double Ef, Jsd, Tc, es;
    if (argc<4)
    {
        fprintf(stderr,"Usage: %s <Ef> <Jsd> <es>\n",argv[0]);
        return 1;
    }
    else
    {
        Ef = atof(argv[1]);
        Jsd = atof(argv[2]);
        es = atof(argv[3]);
    }
    sfindtc_(&Ef, &Jsd, e, t, &Tc);
    printf("%7.2lf\n",Tc*11604);
*/
/*
    // TcEf
    double Jsd=5.68;
    e[1] = 3.997;
    char**fname="x.dat";
    tcef_(&Jsd, e, t);
*/
/*
    //Spectrum
    double px;
    double py;
    double dspec[4];
    int info;
    px = 0.0;
    py = 3.14;
    scanf("%lf%lf%lf",&e[1],&px,&py);
    spec_(&px, &py, e, t, dspec, &info);
    printf("%6.3lf\t%6.3lf\t%6.3lf\t%6.3lf\t%6.3lf\t%6.3lf\n",
            px,py,dspec[0],dspec[1],dspec[2],dspec[3]);
*/

    return (EXIT_SUCCESS);
}

