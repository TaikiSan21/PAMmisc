/* WAVE file manipulation for R
audio R package
Copyright(c) 2008 Simon Urbanek
Modified 2024-07 by Taiki Sakai to add to/from feature
and avoid crashes on some wav files

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use, copy,
modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

    * The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND ON
INFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

The text above constitutes the entire license; however, the
PortAudio community also makes the following non-binding requests:

    * Any person wishing to distribute modifications to the Software is
requested to send the modifications to the original developer so
that they can be incorporated into the canonical version. It is
also requested that these non-binding requests be included along
with the license above.

*/

    #include <stdio.h>
    #include <string.h>

    #define R_NO_REMAP      /* to not pollute the namespace */

    #include <R.h>
    #include <Rinternals.h>

    /* WAVE file is essentially a RIFF file, hence the structures */

    typedef struct riff_header {
        char riff[4]; /* RIFF */
            unsigned int len;
        char type[4]; /* file type (WAVE) for wav */
    } riff_header_t;

typedef struct riff_chunk {
    char rci[4];
    unsigned int len;
} riff_chunk_t;

typedef struct wav_fmt {
    char rci[4]; /* RIFF chunk identifier, "fmt " here */
        unsigned int len;
    short ver, chs;
    unsigned int rate, bps;
    unsigned short byps, bips;
} wav_fmt_t;

SEXP load_wave_file(SEXP src, SEXP from, SEXP to, SEXP header)
{
    if (Rf_inherits(src, "connection"))
        Rf_error("sorry, connections are not supported yet");
    if (TYPEOF(src) != STRSXP || LENGTH(src) < 1)
        Rf_error("invalid file name");
    if (TYPEOF(from) != REALSXP || TYPEOF(to) != REALSXP)
        Rf_error("'from' and 'to' must be numeric");
    if (TYPEOF(header) != INTSXP)
        Rf_error("'header' must be TRUE or FALSE");

    {
        const char *fName = CHAR(STRING_ELT(src, 0));
        FILE *f = fopen(fName, "rb");
        riff_header_t rh;
        wav_fmt_t fmt;
        riff_chunk_t rc;
        unsigned int to_go = 0, has_fmt = 0;
        SEXP res = R_NilValue;

        if (!f)
            Rf_error("unable to open file '%s'", fName);
        if (fread(&rh, sizeof(rh), 1, f) != 1) {
            fclose(f);
            Rf_error("unable to read header from '%s'", fName);
        }
        if (memcmp(rh.riff, "RIFF", 4) || memcmp(rh.type, "WAVE", 4)) {
            fclose(f);
            Rf_error("not a WAVE format");
        }
        to_go = rh.len;
        while (!feof(f) && to_go >= 8) {
            int n = fread(&rc, 1, 8, f);
            if (n < 8) {
                fclose(f);
                Rf_error("incomplete file 1");
            }
            to_go -= n;
            if (!memcmp(rc.rci, "fmt ", 4)) { /* format chunk */
                    if (to_go < 16) {
                        fclose(f);
                        Rf_error("corrupt file");
                    }
                memcpy(&fmt, &rc, 8);
                n = fread(&fmt.ver, 1, 16, f);
                if (n < 16) {
                    fclose(f);
                    Rf_error("incomplete file 2");
                }
                to_go -= n;
                has_fmt = 1;
            } else if (!memcmp(rc.rci, "data", 4)) {

                unsigned int samples = rc.len;
                unsigned int st = 1;

                double *d;
                if (!has_fmt) {
                    fclose(f);
                    Rf_error("data chunk without preceeding format chunk");
                }
                if (fmt.bips == 16) {
                    st = 2; samples /= 2;
                } else if (fmt.bips == 32) {
                    st = 4; samples /= 4;
                } else if (fmt.bips != 8) {
                    fclose(f);
                    Rf_error("unsupported smaple width: %d bits", fmt.bips);
                }

                unsigned int from_samp =  round(REAL(from)[0] *  fmt.rate);
                from_samp *= fmt.chs;
                unsigned int to_samp;

                if (REAL(to)[0] == NA_REAL) {
                    to_samp = samples;
                } else if (REAL(to)[0] * fmt.rate * fmt.chs < samples) {
                    to_samp = round(fmt.rate * REAL(to)[0]);
                    to_samp *= fmt.chs;
                } else {
                    to_samp = samples;
                }
                int isHeader = *INTEGER(header);

                if (isHeader == 1) {
                    SEXP res = PROTECT(Rf_allocVector(VECSXP, 4));
                    SET_VECTOR_ELT(res, 0, Rf_ScalarInteger(fmt.rate));       /* numeric(1) */
                        SET_VECTOR_ELT(res, 1, Rf_ScalarInteger(fmt.chs));   /* numeric(<some length>) */
                        SET_VECTOR_ELT(res, 2, Rf_ScalarInteger(fmt.bips));
                    SET_VECTOR_ELT(res, 3, Rf_ScalarInteger(samples / fmt.chs));
                    UNPROTECT(1);
                    // after this we assign names in R
                    fclose(f);
                    return res;
                }
                samples = to_samp - from_samp;
                fseek(f, from_samp * st, SEEK_CUR);

                res = Rf_allocVector(REALSXP, samples);
                n = fread(d = REAL(res), st, samples, f);

                if (n < samples) {
                    fclose(f);
                    Rf_error("incomplete file 3");
                }
                if (to_go > samples * st) {
                    // Rf_warning("Data does not reach end of file");
                    to_go = samples * st;
                }
                /* now convert the format to doubles, in-place */
                    {
                        int i = n - 1;
                        switch (st) {
                            case 1:
                                {
                                    signed char *ca = (signed char*) REAL(res);
                                    while (i >= 0) {
                                        signed char c = ca[i];
                                        d[i--] = (c < 0)?(((double) c) / 127.0) : (((double) c) / 128.0);
                                    }
                                }
                            case 2:
                                {
                                    short int *sa = (short int*) REAL(res);
                                    while (i >= 0) {
                                        short int s = sa[i];
                                        d[i--] = (s < 0)?(((double) s) / 32767.0) : (((double) s) / 32768.0);
                                    }
                                }
                            case 4:
                                {
                                    int *sa = (int*) REAL(res);
                                    while (i >= 0) {
                                        int s = sa[i];
                                        d[i--] = (s < 0)?(((double) s) / 2147483647.0) : (((double) s) / 2147483648.0);
                                    }
                                }
                        }
                    }
                n *= st;
                if (n > to_go) /* it's questionable whether we should bark here - it's file inconsistency to say the least */
                    to_go = 0;
                else
                    to_go -= n;
            } else { /* skip any chunks we don't know */
				if (rc.len > to_go || fseek(f, rc.len, SEEK_CUR)) {
					fclose(f);
					Rf_error("incomplete file 4");
				}
				to_go -= rc.len;
			}
		}
		fclose(f);
		Rf_protect(res);
		{
			SEXP sym = Rf_protect(Rf_install("rate"));
			Rf_setAttrib(res, sym, Rf_ScalarInteger(fmt.rate));
			Rf_unprotect(1);
			sym = Rf_protect(Rf_install("bits"));
			Rf_setAttrib(res, sym, Rf_ScalarInteger(fmt.bips));
			Rf_unprotect(1);
			Rf_setAttrib(res, R_ClassSymbol, Rf_mkString("audioSample"));
			if (fmt.chs > 1) {
				SEXP dim = Rf_allocVector(INTSXP, 2);
				INTEGER(dim)[0] = fmt.chs;
				INTEGER(dim)[1] = LENGTH(res) / fmt.chs;
				Rf_setAttrib(res, R_DimSymbol, dim);
			}
		}
		Rf_unprotect(1);
		return res;
	}
}
