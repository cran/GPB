#include <stdlib.h>  
#include <R_ext/Rdynload.h>

/* .C calls */
extern void gpb_dft_cf(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);


static const R_CMethodDef CEntries[] = {
    {"gpb_dft_cf",     (DL_FUNC) &gpb_dft_cf,      14},
    {NULL, NULL, 0}
};

void R_init_GPB(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
