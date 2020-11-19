/* model.c
   ___________________________________________________

   Model File:  linear2.model

   Date:  Fri Mar 24 16:26:50 2017

   Created by:  "mod v5.6.6"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2017 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   0 States:

   2 Outputs:
     y -> 0.0;
     z -> 0.0;

   0 Inputs:

   4 Parameters:
     A = 0;
     B = 1;
     SD_y = 0;
     SD_z = 0;
*/


#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <float.h>
#include "modelu.h"
#include "random.h"
#include "yourcode.h"


/*----- Indices to Global Variables */

/* Model variables: States and other outputs */
#define ID_y 0x00000
#define ID_z 0x00001

/* Inputs */

/* Parameters */
#define ID_A 0x00002
#define ID_B 0x00003
#define ID_SD_y 0x00004
#define ID_SD_z 0x00005


/*----- Global Variables */

/* For export. Keep track of who we are. */
char szModelDescFilename[] = "linear2.model";
char szModelSourceFilename[] = __FILE__;
char szModelGenAndVersion[] = "mod v5.6.6";

/* Externs */
extern BOOL vbModelReinitd;

/* Model Dimensions */
int vnStates = 0;
int vnOutputs = 2;
int vnModelVars = 2;
int vnInputs = 0;
int vnParms = 4;

/* States and Outputs*/
double vrgModelVars[2];

/* Inputs */
IFN vrgInputs[1];

/* Parameters */
double A;
double B;
double SD_y;
double SD_z;

BOOL bDelays = 0;


/*----- Global Variable Map */

VMMAPSTRCT vrgvmGlo[] = {
  {"y", (PVOID) &vrgModelVars[ID_y], ID_OUTPUT | ID_y},
  {"z", (PVOID) &vrgModelVars[ID_z], ID_OUTPUT | ID_z},
  {"A", (PVOID) &A, ID_PARM | ID_A},
  {"B", (PVOID) &B, ID_PARM | ID_B},
  {"SD_y", (PVOID) &SD_y, ID_PARM | ID_SD_y},
  {"SD_z", (PVOID) &SD_z, ID_PARM | ID_SD_z},
  {"", NULL, 0} /* End flag */
};  /* vrgpvmGlo[] */


/*----- InitModel
   Should be called to initialize model variables at
   the beginning of experiment before reading
   variants from the simulation spec file.
*/

void InitModel(void)
{
  /* Initialize things in the order that they appear in
     model definition file so that dependencies are
     handled correctly. */

  vrgModelVars[ID_y] = 0.0;
  vrgModelVars[ID_z] = 0.0;
  A = 0;
  B = 1;
  SD_y = 0;
  SD_z = 0;

  vbModelReinitd = TRUE;

} /* InitModel */


/*----- Dynamics section */

void CalcDeriv (double  rgModelVars[], double  rgDerivs[], PDOUBLE pdTime)
{

  CalcInputs (pdTime); /* Get new input vals */


} /* CalcDeriv */


/*----- Model scaling */

void ScaleModel (PDOUBLE pdTime)
{

} /* ScaleModel */


/*----- Jacobian calculations */

void CalcJacob (PDOUBLE pdTime, double rgModelVars[],
                long column, double rgdJac[])
{

} /* CalcJacob */


/*----- Outputs calculations */

void CalcOutputs (double  rgModelVars[], double  rgDerivs[], PDOUBLE pdTime)
{

  rgModelVars[ID_y] = A + B * (*pdTime) ;
  rgModelVars[ID_z] = rgModelVars[ID_y] ;

}  /* CalcOutputs */


