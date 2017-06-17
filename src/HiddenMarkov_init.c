#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Fortran calls */
extern void F77_NAME(loop1)(int *m, int *n, double *phi, double *prob, double *Pi, double *logalp, double *lscale, double *tmp);

extern void F77_NAME(loop2)(int *m, int *n, double *phi, double *prob, double *Pi, double *logbet, double *lscale, double *tmp);

extern void F77_NAME(loop3)(int *m, int *n, double *phi, double *S, double *eigenv, double *logalp, double *scalef, double *tau, double *post, double *psi, double *psi0, double *psi1, double *tmp);

extern void F77_NAME(loop4)(int *m, int *n, double *phi, double *logbet, double *scalef, double *psi, double *psi1, double *tmp);

extern void F77_NAME(loop5)(int *m, int *n, double *d, double *tau, double *scalef, double *diff, double *TT, double *exptau);

extern void F77_NAME(loop6)(int *m, int *n, double *TT, double *S, double *Sinv, double *post0, double *alpha, double *beta, double *A, double *pre, double *post, double *TTi, double *tmp, double *tmp0);



static const R_FortranMethodDef FortranEntries[] = {
    {"loop1", (DL_FUNC) &F77_NAME(loop1),  8},
    {"loop2", (DL_FUNC) &F77_NAME(loop2),  8},
    {"loop3", (DL_FUNC) &F77_NAME(loop3), 13},
    {"loop4", (DL_FUNC) &F77_NAME(loop4),  8},
    {"loop5", (DL_FUNC) &F77_NAME(loop5),  8},
    {"loop6", (DL_FUNC) &F77_NAME(loop6), 14},
    {NULL, NULL, 0}
};

void R_init_HiddenMarkov(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

