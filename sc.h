/* 
 * File:   sc.h
 * Author: bozhin
 *
 * Created on 25.07.2012, 16:41
 */

#ifndef SC_H
#define	SC_H

#ifdef	__cplusplus
extern "C" {
#endif
/*
 * PROTOS
 */
void setriang_();
void getesef_(int*, double*, double*, double*, double*, double*, double*, double*, int*);
void vhenergy_(double*,double*,double*);
void cec_(double*,double*,double*);
void surf_(double*,double*);
void scdos_();
void sfindtc_(double*Ef,double *Jsd, double*e,double*t, double *Tc);
void tcef_(double*Jsd, double*e, double*t);
void spec_(double*px, double*py, double*e, double*t, double*dpec, int*info);
double *findef_(double *temp, double *e, double *t, double *ff);

/*
 * Parameters
 */
#define N 13
/* s-parameter input data*/
double sParam[] =  { 1.74,   2.44,   0.72,   0.87,   0.66,   0.62,   0.59,   0.33,   0.33,   0.39,   0.38,   0.36,   0.94 };
/*Calculated values for Cu4s level*/
double esParam[] = {11.368, 14.840,  5.964,  6.806,  5.620,  5.387,  5.212,  3.614,  3.614,  3.997,  3.934,  3.807,  7.191};
/*Calculated values for Fermi energy level at filling factor \tilde{p}=0.16*/
double efParam[] = { 1.959,  2.053,  1.705,  1.757,  1.681,  1.665,  1.651,  1.512,  1.512,  1.549,  1.543,  1.531,  1.779};
/*Calculated values for van Hove energy level*/
double vhParam[] = { 1.941,  2.078,  1.486,  1.591,  1.437,  1.402,  1.375,  1.075,  1.075,  1.156,  1.143,  1.116,  1.633};


double e[] = {0.0,3.807,-1.0};
double t[] = {1.5, 2.0, 0.2};
double fillingFactor = 0.34;


#ifdef	__cplusplus
}
#endif

#endif	/* SC_H */

