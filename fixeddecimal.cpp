extern "C" {
#include "postgres.h"
#include "libpq/pqformat.h"
#include "access/hash.h"
#include "utils/array.h"
#include "utils/builtins.h"
#include "utils/numeric.h"
}

extern "C" {
PG_MODULE_MAGIC;
}

/*
        Creating our own datatype for storing precision, scale, value and sign
*/

typedef struct FixDecType {
    uint8 precision;
    uint8 scale;
    uint8 sign;
    uint128 value;
} FixedDecimal;

typedef struct FixedDecimalAggState {
    MemoryContext agg_context; /* context we're calculating in */
    int128 N;                  /* count of processed numbers */
    uint8 signofsumX;
    uint8 state_scale;
    uint128 sumX; /* sum of processed numbers */
} FixedDecimalAggState;

struct Res {
    uint128 first;
    uint128 second;
};

typedef long double float128;

/*
        Using int128 for implementation
        Considering max precision to be 38
*/

#ifndef HAVE_BUILTIN_OVERFLOW
#define SAMESIGN(a, b) (((a) < 0) == ((b) < 0))
#endif

#define MAX_PRECISION 38
#define MAXFIXALIGN MAXALIGN64(sizeof(FixedDecimal))
#define ISSet(X) (X == UINT8_MAX)
#define ISSpecial(X) (X > 1 && !ISSet(X))
#define ISPinf(X) (X == 2)
#define ISNinf(X) (X == 3)
#define ISNaN(X) (X == 4)
#define UNCONSTRAINED -1
#define FixedDecimalGetDatum(X) PointerGetDatum(X)
#define DatumGetFixedDecimal(X) (FixedDecimal *)(DatumGetPointer(X))
#define PG_GETARG_FIXDECTYPE(n) (FixedDecimal *)PG_GETARG_POINTER(n)
#define PG_RETURN_FIXDECTYPE(X) return FixedDecimalGetDatum(X)

extern "C" {
PG_FUNCTION_INFO_V1(fixeddecimalin);
PG_FUNCTION_INFO_V1(fixeddecimaltypmodin);
PG_FUNCTION_INFO_V1(fixeddecimaltypmodout);
PG_FUNCTION_INFO_V1(fixeddecimalout);
PG_FUNCTION_INFO_V1(fixeddecimalrecv);
PG_FUNCTION_INFO_V1(fixeddecimalsend);

PG_FUNCTION_INFO_V1(fixeddecimaleq);
PG_FUNCTION_INFO_V1(fixeddecimalne);
PG_FUNCTION_INFO_V1(fixeddecimallt);
PG_FUNCTION_INFO_V1(fixeddecimalgt);
PG_FUNCTION_INFO_V1(fixeddecimalle);
PG_FUNCTION_INFO_V1(fixeddecimalge);
PG_FUNCTION_INFO_V1(fixeddecimal_cmp);
PG_FUNCTION_INFO_V1(fixeddecimal_hash);

PG_FUNCTION_INFO_V1(fixeddecimal_int2_eq);
PG_FUNCTION_INFO_V1(fixeddecimal_int2_ne);
PG_FUNCTION_INFO_V1(fixeddecimal_int2_lt);
PG_FUNCTION_INFO_V1(fixeddecimal_int2_gt);
PG_FUNCTION_INFO_V1(fixeddecimal_int2_le);
PG_FUNCTION_INFO_V1(fixeddecimal_int2_ge);
PG_FUNCTION_INFO_V1(fixeddecimal_int2_cmp);

PG_FUNCTION_INFO_V1(int2_fixeddecimal_eq);
PG_FUNCTION_INFO_V1(int2_fixeddecimal_ne);
PG_FUNCTION_INFO_V1(int2_fixeddecimal_lt);
PG_FUNCTION_INFO_V1(int2_fixeddecimal_gt);
PG_FUNCTION_INFO_V1(int2_fixeddecimal_le);
PG_FUNCTION_INFO_V1(int2_fixeddecimal_ge);
PG_FUNCTION_INFO_V1(int2_fixeddecimal_cmp);

PG_FUNCTION_INFO_V1(fixeddecimal_int4_eq);
PG_FUNCTION_INFO_V1(fixeddecimal_int4_ne);
PG_FUNCTION_INFO_V1(fixeddecimal_int4_lt);
PG_FUNCTION_INFO_V1(fixeddecimal_int4_gt);
PG_FUNCTION_INFO_V1(fixeddecimal_int4_le);
PG_FUNCTION_INFO_V1(fixeddecimal_int4_ge);
PG_FUNCTION_INFO_V1(fixeddecimal_int4_cmp);

PG_FUNCTION_INFO_V1(int4_fixeddecimal_eq);
PG_FUNCTION_INFO_V1(int4_fixeddecimal_ne);
PG_FUNCTION_INFO_V1(int4_fixeddecimal_lt);
PG_FUNCTION_INFO_V1(int4_fixeddecimal_gt);
PG_FUNCTION_INFO_V1(int4_fixeddecimal_le);
PG_FUNCTION_INFO_V1(int4_fixeddecimal_ge);
PG_FUNCTION_INFO_V1(int4_fixeddecimal_cmp);

PG_FUNCTION_INFO_V1(fixeddecimal_numeric_cmp);
PG_FUNCTION_INFO_V1(fixeddecimal_numeric_eq);
PG_FUNCTION_INFO_V1(fixeddecimal_numeric_ne);
PG_FUNCTION_INFO_V1(fixeddecimal_numeric_lt);
PG_FUNCTION_INFO_V1(fixeddecimal_numeric_gt);
PG_FUNCTION_INFO_V1(fixeddecimal_numeric_le);
PG_FUNCTION_INFO_V1(fixeddecimal_numeric_ge);

PG_FUNCTION_INFO_V1(numeric_fixeddecimal_cmp);
PG_FUNCTION_INFO_V1(numeric_fixeddecimal_eq);
PG_FUNCTION_INFO_V1(numeric_fixeddecimal_ne);
PG_FUNCTION_INFO_V1(numeric_fixeddecimal_lt);
PG_FUNCTION_INFO_V1(numeric_fixeddecimal_gt);
PG_FUNCTION_INFO_V1(numeric_fixeddecimal_le);
PG_FUNCTION_INFO_V1(numeric_fixeddecimal_ge);

PG_FUNCTION_INFO_V1(int4fixeddecimal);
PG_FUNCTION_INFO_V1(int2fixeddecimal);
PG_FUNCTION_INFO_V1(dtofixeddecimal);
PG_FUNCTION_INFO_V1(ftofixeddecimal);
PG_FUNCTION_INFO_V1(numeric_fixeddecimal);
PG_FUNCTION_INFO_V1(fixeddecimalint4);
PG_FUNCTION_INFO_V1(fixeddecimalint2);
PG_FUNCTION_INFO_V1(fixeddecimaltod);
PG_FUNCTION_INFO_V1(fixeddecimaltof);
PG_FUNCTION_INFO_V1(fixeddecimal_numeric);
PG_FUNCTION_INFO_V1(fixeddecimal);

PG_FUNCTION_INFO_V1(fixeddecimalum);
PG_FUNCTION_INFO_V1(fixeddecimalup);
PG_FUNCTION_INFO_V1(fixeddecimalpl);
PG_FUNCTION_INFO_V1(fixeddecimalmi);
PG_FUNCTION_INFO_V1(fixeddecimalmul);
PG_FUNCTION_INFO_V1(fixeddecimaldiv);

PG_FUNCTION_INFO_V1(fixeddecimalint4pl);
PG_FUNCTION_INFO_V1(fixeddecimalint4mi);
PG_FUNCTION_INFO_V1(fixeddecimalint4mul);
PG_FUNCTION_INFO_V1(fixeddecimalint4div);

PG_FUNCTION_INFO_V1(int4fixeddecimalpl);
PG_FUNCTION_INFO_V1(int4fixeddecimalmi);
PG_FUNCTION_INFO_V1(int4fixeddecimalmul);
PG_FUNCTION_INFO_V1(int4fixeddecimaldiv);

PG_FUNCTION_INFO_V1(fixeddecimalint2pl);
PG_FUNCTION_INFO_V1(fixeddecimalint2mi);
PG_FUNCTION_INFO_V1(fixeddecimalint2mul);
PG_FUNCTION_INFO_V1(fixeddecimalint2div);

PG_FUNCTION_INFO_V1(int2fixeddecimalpl);
PG_FUNCTION_INFO_V1(int2fixeddecimalmi);
PG_FUNCTION_INFO_V1(int2fixeddecimalmul);
PG_FUNCTION_INFO_V1(int2fixeddecimaldiv);

PG_FUNCTION_INFO_V1(fixeddecimalnumericpl);
PG_FUNCTION_INFO_V1(fixeddecimalnumericmi);
PG_FUNCTION_INFO_V1(fixeddecimalnumericmul);
PG_FUNCTION_INFO_V1(fixeddecimalnumericdiv);

PG_FUNCTION_INFO_V1(numericfixeddecimalpl);
PG_FUNCTION_INFO_V1(numericfixeddecimalmi);
PG_FUNCTION_INFO_V1(numericfixeddecimalmul);
PG_FUNCTION_INFO_V1(numericfixeddecimaldiv);

PG_FUNCTION_INFO_V1(fixeddecimalabs);
PG_FUNCTION_INFO_V1(fixeddecimallarger);
PG_FUNCTION_INFO_V1(fixeddecimalsmaller);

PG_FUNCTION_INFO_V1(fixeddecimal_avg_accum);
PG_FUNCTION_INFO_V1(fixeddecimal_avg);
PG_FUNCTION_INFO_V1(fixeddecimal_sum);
PG_FUNCTION_INFO_V1(fixeddecimalaggstatecombine);
PG_FUNCTION_INFO_V1(fixeddecimalaggstateserialize);
PG_FUNCTION_INFO_V1(fixeddecimalaggstatedeserialize);

PG_FUNCTION_INFO_V1(fixeddecimalaggstatein);
PG_FUNCTION_INFO_V1(fixeddecimalaggstateout);
PG_FUNCTION_INFO_V1(fixeddecimalaggstatesend);
PG_FUNCTION_INFO_V1(fixeddecimalaggstaterecv);
}

static char *pg_int64tostr(char *str, int128 value);
static char *pg_int64tostr_zeropad(char *str, int128 value, int128 padding);
static char *fixeddecimal2str(FixedDecimal *val, char *buffer);
static void apply_typmod(FixedDecimal *value, int32 typmod, int precision, int scale);
static void scan_arg_to_fixeddecimal(char *cp, FixedDecimal *arg);
static inline Res get_appropriate_values(uint128 val1, uint128 val2, int128 multiplier);
static uint8 get_scale_difference(uint8 arg1, uint8 arg2);
static uint8 get_precision(uint128 value);
static uint128 get_multiplier(uint8 scale_difference);
static void scanfixeddecimal(FixedDecimal *result, const char *str, int *precision, int *scale, int32 typmod);

/*
    get_appropriate_values :
        *Function used during relational operations between two fixeddecimal arguments.
        *Multiplies the smaller value with multiplier for relational operations.
        *return std::pair as a result.
*/

static inline Res get_appropriate_values(uint128 val1, uint128 val2, int128 multiplier) {
    return (val1 > val2) ? Res{val1, val2 * multiplier} : Res{val1 * multiplier, val2};
}

static uint8 get_scale_difference(uint8 arg1, uint8 arg2) { return (arg1 > arg2) ? arg1 - arg2 : arg2 - arg1; }

static uint8 get_precision(uint128 value) {
    uint8 precision = 0;
    while (value > 0) {
        precision++;
        value = value / 10;
    }
    return precision;
}

static uint128 get_multiplier(uint8 scale_difference) {
    uint128 multiplier = 1;
    while (scale_difference > 0) {
        multiplier *= 10;
        scale_difference--;
    }
    return multiplier;
}
/*
        While creating the column of fixeddecimal type
*/

/*
    fixeddecimaltypmodin :
        Creates fixeddecimal type column in table with precision and scale.
*/

Datum fixeddecimaltypmodin(PG_FUNCTION_ARGS) {
    ArrayType *ta = PG_GETARG_ARRAYTYPE_P(0);
    int32 *tl;
    int n;
    int32 typmod;

    tl = ArrayGetIntegerTypmods(ta, &n);

    if (n == 2) {

        if (tl[0] < 0 || tl[0] > MAX_PRECISION)
            ereport(ERROR, (errcode(ERRCODE_INVALID_PARAMETER_VALUE),
                            errmsg("FIXEDDECIMAL precision %d must be between %d and %d", tl[0], 0, MAX_PRECISION)));

        if (tl[1] < 0)
            ereport(ERROR,
                    (errcode(ERRCODE_FEATURE_NOT_SUPPORTED), errmsg("FIXEDDECIMAL scale must be atleast %d", 0)));

        typmod = ((tl[0] << 16) | tl[1]) + VARHDRSZ;
    } else if (n == 1) {
        if (tl[0] < 0 || tl[0] > MAX_PRECISION)
            ereport(ERROR, (errcode(ERRCODE_INVALID_PARAMETER_VALUE),
                            errmsg("FIXEDDECIMAL precision %d must be between %d and %d", tl[0], 0, MAX_PRECISION)));

        typmod = (tl[0] << 16) + VARHDRSZ;
    } else {
        elog(INFO, "%s", "Unconstrained");
        typmod = (MAX_PRECISION << 16) + VARHDRSZ;
    }

    PG_RETURN_INT32(typmod);
}

/*
    fixeddecimaltypmodout :
        Used to present the table column properties.
*/

Datum fixeddecimaltypmodout(PG_FUNCTION_ARGS) {
    int32 typmod = PG_GETARG_INT32(0);
    char *res = (char *)palloc(64);

    if (typmod >= 0)
        snprintf(res, 64, "(%d,%d)", ((typmod - VARHDRSZ) >> 16) & 0xffff, (typmod - VARHDRSZ) & 0xffff);
    else
        *res = '\0';

    PG_RETURN_CSTRING(res);
}

/*
        While inserting entries into the relation
*/

/*
    fixeddecimalin :
        Taking string as an input and scanning fixeddecimal number.
*/

Datum fixeddecimalin(PG_FUNCTION_ARGS) {
    char *str = PG_GETARG_CSTRING(0);
    int32 typmod = PG_GETARG_INT32(2);
    int precision = 0;
    int scale = 0;
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);
    memset(result, 0, MAXFIXALIGN);
    scanfixeddecimal(result, str, &precision, &scale, typmod);

    apply_typmod(result, typmod, precision - scale, scale);
    PG_RETURN_FIXDECTYPE(result);
}

static void scanfixeddecimal(FixedDecimal *result, const char *str, int *precision, int *scale, int32 typmod) {
    bool negative = false;
    bool isSpecial = false;
    const char *ptr = str;
    int vprecision = 0;
    int vscale = 0;
    int precisionlimit = 0;
    int scalelimit = 0;
    int128 integralpart = 0;
    int128 fractionalpart = 0;
    int128 multiplier = 1;
    int128 tmp_multiplier = 1;

    /* special value check */
    if (typmod == UNCONSTRAINED && isdigit((unsigned char)*ptr) == 0 && isdigit((unsigned char)*(ptr + 1)) == 0) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (pg_strncasecmp(ptr, "NaN", 3) == 0) {
            result->sign = 4;
            isSpecial = true;
        } else if (pg_strncasecmp(ptr, "Infinity", 8) == 0) {
            result->sign = 2;
            isSpecial = true;
        } else if (pg_strncasecmp(ptr, "+Infinity", 9) == 0) {
            result->sign = 2;
            isSpecial = true;
        } else if (pg_strncasecmp(ptr, "-Infinity", 9) == 0) {
            result->sign = 3;
            isSpecial = true;
        } else if (pg_strncasecmp(ptr, "inf", 3) == 0) {
            result->sign = 2;
            isSpecial = true;
        } else if (pg_strncasecmp(ptr, "+inf", 4) == 0) {
            result->sign = 2;
            isSpecial = true;
        } else if (pg_strncasecmp(ptr, "-inf", 4) == 0) {
            result->sign = 3;
            isSpecial = true;
        }
    }

    if (!isSpecial) {
        /* skip leading spaces */
        while (isspace((unsigned char)*ptr))
            ptr++;

        /* handle sign */
        if (*ptr == '-') {
            negative = true;
            result->sign = -1;
            ptr++;

            while (isdigit((unsigned char)*ptr)) {
                int128 tmp = integralpart * 10 - (*ptr++ - '0');

                vprecision++;
                if ((tmp / 10) != integralpart) /* underflow? */
                {
                    ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE),
                                    errmsg("value \"%s\" is out of range for type fixeddecimal", str)));
                }
                integralpart = tmp;
            }
        } else {
            negative = false;
            result->sign = 1;
            if (*ptr == '+')
                ptr++;

            while (isdigit((unsigned char)*ptr)) {
                int128 tmp = integralpart * 10 + (*ptr++ - '0');

                vprecision++;
                if ((tmp / 10) != integralpart) /* overflow? */
                {
                    ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE),
                                    errmsg("value \"%s\" is out of range for type fixeddecimal", str)));
                }
                integralpart = tmp;
            }
        }

        if (typmod > 0) {
            typmod -= VARHDRSZ;
            precisionlimit = (typmod >> 16) & 0xffff;
            scalelimit = typmod & 0xffff;
            vscale = scalelimit;

            result->precision = precisionlimit;
            result->scale = scalelimit;

            while (vscale > 0) {
                multiplier = multiplier * 10;
                vscale--;
            }
        } else {
            /* when typmod = -1 */
            int str_len;
            str_len = strlen(str);
            precisionlimit = vprecision;

            if (integralpart < 0) {
                scalelimit = str_len - precisionlimit - 2;
            } else {
                scalelimit = str_len - precisionlimit - 1;
            }

            vscale = scalelimit;
            if (scalelimit > 0) {
                result->scale = scalelimit;
            } else {
                result->scale = 0;
            }

            while (vscale > 0) {
                multiplier = multiplier * 10;
                vscale--;
            }
        }

        /* process the part after the decimal point */
        if (*ptr == '.') {
            ptr++;
            tmp_multiplier = multiplier;
            while (isdigit((unsigned char)*ptr) && tmp_multiplier > 1) {
                vprecision++;
                tmp_multiplier /= 10;
                fractionalpart += (*ptr++ - '0') * tmp_multiplier;
            }

            /*
             * Eat into any excess precision digits.
             * XXX These are ignored, should we error instead?
             */
            while (isdigit((unsigned char)*ptr))
                ptr++;
        }

        /* consume any remaining space chars */
        while (isspace((unsigned char)*ptr))
            ptr++;

        if (*ptr != '\0')
            ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE),
                            errmsg("value \"%s\" is out of range for type fixeddecimal", str)));

        *precision = vprecision;
        *scale = scalelimit;
        result->precision = vprecision;

        if (negative) {

            int128 value = 0;

#ifdef HAVE_BUILTIN_OVERFLOW
            int64 multiplier = FIXEDDECIMAL_MULTIPLIER;
            if (__builtin_mul_overflow(integralpart, multiplier, &value))
                ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE),
                                errmsg("value \"%s\" is out of range for type fixeddecimal", str)));

            if (__builtin_sub_overflow(value, fractionalpart, &value))
                ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE),
                                errmsg("value \"%s\" is out of range for type fixeddecimal", str)));
            return value;

#else
            value = integralpart * multiplier;
            value = -1 * (value - fractionalpart);
            result->value = value;
#endif /* HAVE_BUILTIN_OVERFLOW */

        } else {
            int128 value = 0;

#ifdef HAVE_BUILTIN_OVERFLOW
            if (__builtin_mul_overflow(integralpart, FIXEDDECIMAL_MULTIPLIER, &value))
                ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE),
                                errmsg("value \"%s\" is out of range for type fixeddecimal", str)));

            if (__builtin_add_overflow(value, fractionalpart, &value))
                ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE),
                                errmsg("value \"%s\" is out of range for type fixeddecimal", str)));
            return value;
#else
            value = integralpart * multiplier;
            value = value + fractionalpart;
            result->value = value;
#endif /* HAVE_BUILTIN_OVERFLOW */
        }
    }
}

static void apply_typmod(FixedDecimal *value, int32 typmod, int precision, int scale) {
    int precisionlimit;
    int scalelimit;
    int maxdigits;

    /* Do nothing if we have a default typmod (-1) */
    if (typmod < (int32)(VARHDRSZ))
        return;

    typmod -= VARHDRSZ;
    precisionlimit = (typmod >> 16) & 0xffff;
    scalelimit = typmod & 0xffff;
    maxdigits = precisionlimit - scalelimit;

    if (scale > scalelimit)

        if (scale != value->scale)
            ereport(ERROR,
                    (errcode(ERRCODE_FEATURE_NOT_SUPPORTED), errmsg("FIXEDDECIMAL scale must be %d", value->scale)));

    if (precision > maxdigits)
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("FIXEDDECIMAL field overflow"),
                        errdetail("A field with precision %d, scale %d must round to an absolute value less than %s%d.",
                                  precision, scale,
                                  /* Display 10^0 as 1 */
                                  maxdigits ? "10^" : "", maxdigits ? maxdigits : 1)));
}

/*
        During query execution
*/

Datum fixeddecimalout(PG_FUNCTION_ARGS) {
    FixedDecimal *result;
    char *end;
    char buf[MAXFIXALIGN + 1];
    result = PG_GETARG_FIXDECTYPE(0);
    end = fixeddecimal2str(result, buf);
    if (result->sign > 1 && !ISSet(result->sign)) {
        PG_RETURN_CSTRING(end);
    }
    PG_RETURN_CSTRING(pnstrdup(buf, end - buf));
}

static char *fixeddecimal2str(FixedDecimal *val, char *buffer) {
    char *ptr = buffer;

    if (val->sign == 2) {
        ptr = pstrdup("Infinity");
    } else if (val->sign == 3) {
        ptr = pstrdup("-Infinity");
    } else if (val->sign == 4) {
        ptr = pstrdup("NaN");
    } else {
        int128 multiplier = 1;
        int128 integralpart;
        int128 fractionalpart;
        for (int i = 0; i < val->scale; i++) {
            multiplier *= 10;
        }
        integralpart = val->value / multiplier;
        fractionalpart = val->value % multiplier;

        if (ISSet(val->sign)) {
            integralpart = -integralpart;
            /*
             * Handle special case for negative numbers where the intergral part
             * is zero. pg_int64tostr() won't prefix with "-0" in this case, so
             * we'll do it manually
             */
            if (integralpart == 0)
                *ptr++ = '-';
        }
        ptr = pg_int64tostr(ptr, integralpart);
        if (fractionalpart > 0) {
            *ptr++ = '.';
            ptr = pg_int64tostr_zeropad(ptr, fractionalpart, val->scale);
        }
    }
    return ptr;
}

static char *pg_int64tostr(char *str, int128 value) {
    char *start;
    char *end;

    /*
     * Handle negative numbers in a special way. We can't just append a '-'
     * prefix and reverse the sign as on two's complement machines negative
     * numbers can be 1 further from 0 than positive numbers, we do it this way
     * so we properly handle the smallest possible value.
     */
    if (value < 0) {
        *str++ = '-';

        /* mark the position we must reverse the string from. */
        start = str;

        /* Compute the result string backwards. */
        do {
            int128 remainder;
            int128 oldval = value;

            value /= 10;
            remainder = oldval - value * 10;
            *str++ = '0' + -remainder;
        } while (value != 0);
    } else {
        /* mark the position we must reverse the string from. */
        start = str;
        do {
            int64 remainder;
            int64 oldval = value;

            value /= 10;
            remainder = oldval - value * 10;
            *str++ = '0' + remainder;
        } while (value != 0);
    }

    /* Add trailing NUL byte, and back up 'str' to the last character. */
    end = str;
    *str-- = '\0';

    /* Reverse string. */
    while (start < str) {
        char swap = *start;
        *start++ = *str;
        *str-- = swap;
    }
    return end;
}

static char *pg_int64tostr_zeropad(char *str, int128 value, int128 padding) {
    char *start = str;
    char *end = &str[padding];
    int128 num = value;

    /*
     * Handle negative numbers in a special way. We can't just append a '-'
     * prefix and reverse the sign as on two's complement machines negative
     * numbers can be 1 further from 0 than positive numbers, we do it this way
     * so we properly handle the smallest possible value.
     */
    if (num < 0) {
        padding--;

        /*
         * Build the number starting at the end. Here remainder will be a
         * negative number, we must reverse this sign on this before adding
         * '0' in order to get the correct ASCII digit
         */
        while (padding--) {
            int128 remainder;
            int128 oldval = num;

            num /= 10;
            remainder = oldval - num * 10;
            start[padding] = '0' + -remainder;
        }
    } else {
        /* build the number starting at the end */
        while (padding--) {
            int128 remainder;
            int128 oldval = num;

            num /= 10;
            remainder = oldval - num * 10;
            start[padding] = '0' + remainder;
        }
    }

    /*
     * If padding was not high enough to fit this number then num won't have
     * been divided down to zero. We'd better have another go, this time we
     * know there won't be any zero padding required so we can just enlist the
     * help of pg_int64tostr()
     */
    if (num != 0)
        return pg_int64tostr(str, value);

    *end = '\0';
    return end;
}

Datum fixeddecimalrecv(PG_FUNCTION_ARGS) {
    StringInfo buf = (StringInfo)PG_GETARG_POINTER(0);

    PG_RETURN_INT64(pq_getmsgint64(buf));
}

Datum fixeddecimalsend(PG_FUNCTION_ARGS) {
    int64 arg1 = PG_GETARG_INT64(0);
    StringInfoData buf;

    pq_begintypsend(&buf);
    pq_sendint64(&buf, arg1);
    PG_RETURN_BYTEA_P(pq_endtypsend(&buf));
}

/*----------------------------------------------------------
 *	Relational operators for fixeddecimals, including cross-data-type comparisons.
 *---------------------------------------------------------*/

Datum fixeddecimaleq(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    uint8 scale_difference = 0;
    int128 arg1;
    int128 arg2;
    int128 multiplier = 1;
    scale_difference = get_scale_difference(val1->scale, val2->scale);
    multiplier = get_multiplier(scale_difference);
    auto res = get_appropriate_values(val1->value, val2->value, multiplier);
    arg1 = res.first;
    arg2 = res.second;

    if (ISSet(val1->sign) && ISSet(val2->sign)) {
        PG_RETURN_BOOL(-1 * (arg1) == -1 * (arg2));
    } else if (ISSet(val1->sign) && !ISSet(val2->sign)) {
        PG_RETURN_BOOL(-1 * (arg1) == (arg2));
    } else if (!ISSet(val1->sign) && ISSet(val2->sign)) {
        PG_RETURN_BOOL(arg1 == -1 * (arg2));
    } else {
        PG_RETURN_BOOL(arg1 == arg2);
    }
}

Datum fixeddecimalne(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    uint8 scale_difference = 0;
    int128 arg1;
    int128 arg2;
    int128 multiplier = 1;
    scale_difference = get_scale_difference(val1->scale, val2->scale);
    multiplier = get_multiplier(scale_difference);
    auto res = get_appropriate_values(val1->value, val2->value, multiplier);
    arg1 = res.first;
    arg2 = res.second;

    if (ISSet(val1->sign) && ISSet(val2->sign)) {
        PG_RETURN_BOOL(-1 * (arg1) != -1 * (arg2));
    } else if (ISSet(val1->sign) && !ISSet(val2->sign)) {
        PG_RETURN_BOOL(-1 * (arg1) != (arg2));
    } else if (!ISSet(val1->sign) && ISSet(val2->sign)) {
        PG_RETURN_BOOL(arg1 != -1 * (arg2));
    } else {
        PG_RETURN_BOOL(arg1 != arg2);
    }
}

Datum fixeddecimallt(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    uint8 scale_difference = 0;
    int128 arg1;
    int128 arg2;
    int128 multiplier = 1;
    scale_difference = get_scale_difference(val1->scale, val2->scale);
    multiplier = get_multiplier(scale_difference);
    auto res = get_appropriate_values(val1->value, val2->value, multiplier);
    arg1 = res.first;
    arg2 = res.second;

    if (ISSet(val1->sign) && ISSet(val2->sign)) {
        PG_RETURN_BOOL(-1 * (arg1) < -1 * (arg2));
    } else if (ISSet(val1->sign) && !ISSet(val2->sign)) {
        PG_RETURN_BOOL(-1 * (arg1) < (arg2));
    } else if (!ISSet(val1->sign) && ISSet(val2->sign)) {
        PG_RETURN_BOOL(arg1 < -1 * (arg2));
    } else {
        PG_RETURN_BOOL(arg1 < arg2);
    }
}

Datum fixeddecimalgt(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    uint8 scale_difference = 0;
    int128 arg1;
    int128 arg2;
    int128 multiplier = 1;
    scale_difference = get_scale_difference(val1->scale, val2->scale);
    multiplier = get_multiplier(scale_difference);
    auto res = get_appropriate_values(val1->value, val2->value, multiplier);
    arg1 = res.first;
    arg2 = res.second;

    if (ISSet(val1->sign) && ISSet(val2->sign)) {
        PG_RETURN_BOOL(-1 * (arg1) > -1 * (arg2));
    } else if (ISSet(val1->sign) && !ISSet(val2->sign)) {
        PG_RETURN_BOOL(-1 * (arg1) > (arg2));
    } else if (!ISSet(val1->sign) && ISSet(val2->sign)) {
        PG_RETURN_BOOL(arg1 > -1 * (arg2));
    } else {
        PG_RETURN_BOOL(arg1 > arg2);
    }
}

Datum fixeddecimalle(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    uint8 scale_difference = 0;
    int128 arg1;
    int128 arg2;
    int128 multiplier = 1;
    scale_difference = get_scale_difference(val1->scale, val2->scale);
    multiplier = get_multiplier(scale_difference);
    auto res = get_appropriate_values(val1->value, val2->value, multiplier);
    arg1 = res.first;
    arg2 = res.second;

    if (ISSet(val1->sign) && ISSet(val2->sign)) {
        PG_RETURN_BOOL(-1 * (arg1) <= -1 * (arg2));
    } else if (ISSet(val1->sign) && !ISSet(val2->sign)) {
        PG_RETURN_BOOL(-1 * (arg1) <= (arg2));
    } else if (!ISSet(val1->sign) && ISSet(val2->sign)) {
        PG_RETURN_BOOL(arg1 <= -1 * (arg2));
    } else {
        PG_RETURN_BOOL(arg1 <= arg2);
    }
}

Datum fixeddecimalge(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    uint8 scale_difference = 0;
    int128 arg1;
    int128 arg2;
    int128 multiplier = 1;
    scale_difference = get_scale_difference(val1->scale, val2->scale);
    multiplier = get_multiplier(scale_difference);
    auto res = get_appropriate_values(val1->value, val2->value, multiplier);
    arg1 = res.first;
    arg2 = res.second;

    if (ISSet(val1->sign) && ISSet(val2->sign)) {
        PG_RETURN_BOOL(-1 * (arg1) >= -1 * (arg2));
    } else if (ISSet(val1->sign) && !ISSet(val2->sign)) {
        PG_RETURN_BOOL(-1 * (arg1) >= (arg2));
    } else if (!ISSet(val1->sign) && ISSet(val2->sign)) {
        PG_RETURN_BOOL(arg1 >= -1 * (arg2));
    } else {
        PG_RETURN_BOOL(arg1 >= arg2);
    }
}

Datum fixeddecimal_cmp(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int val1sign = 1;
    int val2sign = 1;
    int128 arg1;
    int128 arg2;
    int scale_difference = 0;
    int multiplier = 1;
    scale_difference = get_scale_difference(val1->scale, val2->scale);
    multiplier = get_multiplier(scale_difference);
    auto res = get_appropriate_values(val1->value, val2->value, multiplier);
    arg1 = res.first;
    arg2 = res.second;

    if (ISSet(val1->sign)) {
        val1sign = -1;
    }

    if (ISSet(val2->sign)) {
        val2sign = -1;
    }

    if ((val1sign) * (arg1) == (val2sign) * (arg2))
        PG_RETURN_INT32(0);
    else if ((val1sign) * (arg1) < (val2sign) * (arg2))
        PG_RETURN_INT32(-1);
    else
        PG_RETURN_INT32(1);
}

Datum fixeddecimal_hash(PG_FUNCTION_ARGS) {
    FixedDecimal *val = PG_GETARG_FIXDECTYPE(0);
    Datum result;
    int128 arg = val->value;
    arg = (ISSet(val->sign)) ? -1 * arg : arg;
    result = hash_any((unsigned char *)&(arg), sizeof(int64));
    PG_RETURN_DATUM(result);
}

/* int2, fixeddecimal */
Datum fixeddecimal_int2_eq(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    int128 val2 = PG_GETARG_INT16(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val1sign = 1;

    if (ISSet(val1->sign)) {
        val1sign = -1;
    }

    multiplier = get_multiplier(val1->scale);
    val2 = val2 * multiplier;
    arg = val1sign * (val1->value);
    PG_RETURN_BOOL(arg == val2);
}

Datum fixeddecimal_int2_ne(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    int128 val2 = PG_GETARG_INT16(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val1sign = 1;

    if (ISSet(val1->sign)) {
        val1sign = -1;
    }

    multiplier = get_multiplier(val1->scale);
    val2 = val2 * multiplier;
    arg = val1sign * (val1->value);
    PG_RETURN_BOOL(arg != val2);
}

Datum fixeddecimal_int2_lt(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    int128 val2 = PG_GETARG_INT16(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val1sign = 1;

    if (ISSet(val1->sign)) {
        val1sign = -1;
    }

    multiplier = get_multiplier(val1->scale);
    val2 = val2 * multiplier;
    arg = val1sign * (val1->value);
    PG_RETURN_BOOL(arg < val2);
}

Datum fixeddecimal_int2_gt(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    int128 val2 = PG_GETARG_INT16(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val1sign = 1;

    if (ISSet(val1->sign)) {
        val1sign = -1;
    }

    multiplier = get_multiplier(val1->scale);
    val2 = val2 * multiplier;
    arg = val1sign * (val1->value);
    PG_RETURN_BOOL(arg > val2);
}

Datum fixeddecimal_int2_le(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    int128 val2 = PG_GETARG_INT16(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val1sign = 1;

    if (ISSet(val1->sign)) {
        val1sign = -1;
    }

    multiplier = get_multiplier(val1->scale);
    val2 = val2 * multiplier;
    arg = val1sign * (val1->value);
    PG_RETURN_BOOL(arg <= val2);
}

Datum fixeddecimal_int2_ge(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    int128 val2 = PG_GETARG_INT16(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val1sign = 1;

    if (ISSet(val1->sign)) {
        val1sign = -1;
    }

    multiplier = get_multiplier(val1->scale);
    val2 = val2 * multiplier;
    arg = val1sign * (val1->value);
    PG_RETURN_BOOL(arg >= val2);
}

Datum fixeddecimal_int2_cmp(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    int128 val2 = PG_GETARG_INT16(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val1sign = 1;

    if (ISSet(val1->sign)) {
        val1sign = -1;
    }

    multiplier = get_multiplier(val1->scale);
    val2 = val2 * multiplier;
    arg = val1sign * (val1->value);
    if (arg == val2)
        PG_RETURN_INT32(0);
    else if (arg < val2)
        PG_RETURN_INT32(-1);
    else
        PG_RETURN_INT32(1);
}

Datum int2_fixeddecimal_eq(PG_FUNCTION_ARGS) {
    int128 val1 = PG_GETARG_INT16(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val2sign = 1;

    if (ISSet(val2->sign)) {
        val2sign = -1;
    }

    multiplier = get_multiplier(val2->scale);
    val1 = val1 * multiplier;
    arg = val2sign * (val2->value);
    PG_RETURN_BOOL(val1 == arg);
}

Datum int2_fixeddecimal_ne(PG_FUNCTION_ARGS) {
    int128 val1 = PG_GETARG_INT16(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val2sign = 1;

    if (ISSet(val2->sign)) {
        val2sign = -1;
    }

    multiplier = get_multiplier(val2->scale);
    val1 = val1 * multiplier;
    arg = val2sign * (val2->value);
    PG_RETURN_BOOL(val1 != arg);
}

Datum int2_fixeddecimal_lt(PG_FUNCTION_ARGS) {
    int128 val1 = PG_GETARG_INT16(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val2sign = 1;

    if (ISSet(val2->sign)) {
        val2sign = -1;
    }

    multiplier = get_multiplier(val2->scale);
    val1 = val1 * multiplier;
    arg = val2sign * (val2->value);
    PG_RETURN_BOOL(val1 < arg);
}

Datum int2_fixeddecimal_gt(PG_FUNCTION_ARGS) {
    int128 val1 = PG_GETARG_INT16(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val2sign = 1;

    if (ISSet(val2->sign)) {
        val2sign = -1;
    }

    multiplier = get_multiplier(val2->scale);
    val1 = val1 * multiplier;
    arg = val2sign * (val2->value);
    PG_RETURN_BOOL(val1 > arg);
}

Datum int2_fixeddecimal_le(PG_FUNCTION_ARGS) {
    int128 val1 = PG_GETARG_INT16(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val2sign = 1;

    if (ISSet(val2->sign)) {
        val2sign = -1;
    }

    multiplier = get_multiplier(val2->scale);
    val1 = val1 * multiplier;
    arg = val2sign * (val2->value);
    PG_RETURN_BOOL(val1 <= arg);
}

Datum int2_fixeddecimal_ge(PG_FUNCTION_ARGS) {
    int128 val1 = PG_GETARG_INT16(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val2sign = 1;

    if (ISSet(val2->sign)) {
        val2sign = -1;
    }

    multiplier = get_multiplier(val2->scale);
    val1 = val1 * multiplier;
    arg = val2sign * (val2->value);
    PG_RETURN_BOOL(val1 >= arg);
}

Datum int2_fixeddecimal_cmp(PG_FUNCTION_ARGS) {
    int128 val1 = PG_GETARG_INT16(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int val2sign = 1;
    int128 multiplier = 1;
    int128 arg = val1;

    if (ISSet(val2->sign)) {
        val2sign = -1;
    }

    multiplier = get_multiplier(val2->scale);
    val1 = val1 * multiplier;
    arg = val2sign * (val2->value);
    if (val1 == arg)
        PG_RETURN_INT32(0);
    else if (val1 < arg)
        PG_RETURN_INT32(-1);
    else
        PG_RETURN_INT32(1);
}

/* fixeddecimal, int4 */
Datum fixeddecimal_int4_eq(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    int128 val2 = PG_GETARG_INT32(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val1sign = 1;

    if (ISSet(val1->sign)) {
        val1sign = -1;
    }

    multiplier = get_multiplier(val1->scale);
    val2 = val2 * multiplier;
    arg = val1sign * (val1->value);
    PG_RETURN_BOOL(arg == val2);
}

Datum fixeddecimal_int4_ne(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    int128 val2 = PG_GETARG_INT32(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val1sign = 1;

    if (ISSet(val1->sign)) {
        val1sign = -1;
    }

    multiplier = get_multiplier(val1->scale);
    val2 = val2 * multiplier;
    arg = val1sign * (val1->value);
    PG_RETURN_BOOL(arg != val2);
}

Datum fixeddecimal_int4_lt(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    int128 val2 = PG_GETARG_INT32(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val1sign = 1;

    if (ISSet(val1->sign)) {
        val1sign = -1;
    }

    multiplier = get_multiplier(val1->scale);
    val2 = val2 * multiplier;
    arg = val1sign * (val1->value);
    PG_RETURN_BOOL(arg < val2);
}

Datum fixeddecimal_int4_gt(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    int128 val2 = PG_GETARG_INT32(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val1sign = 1;

    if (ISSet(val1->sign)) {
        val1sign = -1;
    }

    multiplier = get_multiplier(val1->scale);
    val2 = val2 * multiplier;
    arg = val1sign * (val1->value);
    PG_RETURN_BOOL(arg > val2);
}

Datum fixeddecimal_int4_le(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    int128 val2 = PG_GETARG_INT32(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val1sign = 1;

    if (ISSet(val1->sign)) {
        val1sign = -1;
    }

    multiplier = get_multiplier(val1->scale);
    val2 = val2 * multiplier;
    arg = val1sign * (val1->value);
    PG_RETURN_BOOL(arg <= val2);
}

Datum fixeddecimal_int4_ge(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    int128 val2 = PG_GETARG_INT32(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val1sign = 1;

    if (ISSet(val1->sign)) {
        val1sign = -1;
    }

    multiplier = get_multiplier(val1->scale);
    val2 = val2 * multiplier;
    arg = val1sign * (val1->value);
    PG_RETURN_BOOL(arg >= val2);
}

Datum fixeddecimal_int4_cmp(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    int128 val2 = PG_GETARG_INT32(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val1sign = 1;

    if (ISSet(val1->sign)) {
        val1sign = -1;
    }

    multiplier = get_multiplier(val1->scale);
    val2 = val2 * multiplier;
    arg = val1sign * (val1->value);
    if (arg == val2)
        PG_RETURN_INT32(0);
    else if (arg < val2)
        PG_RETURN_INT32(-1);
    else
        PG_RETURN_INT32(1);
}

Datum int4_fixeddecimal_eq(PG_FUNCTION_ARGS) {
    int128 val1 = PG_GETARG_INT32(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val2sign = 1;

    if (ISSet(val2->sign)) {
        val2sign = -1;
    }

    multiplier = get_multiplier(val2->scale);
    val1 = val1 * multiplier;
    arg = val2sign * (val2->value);
    PG_RETURN_BOOL(val1 == arg);
}

Datum int4_fixeddecimal_ne(PG_FUNCTION_ARGS) {
    int128 val1 = PG_GETARG_INT32(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val2sign = 1;

    if (ISSet(val2->sign)) {
        val2sign = -1;
    }

    multiplier = get_multiplier(val2->scale);
    val1 = val1 * multiplier;
    arg = val2sign * (val2->value);
    PG_RETURN_BOOL(val1 != arg);
}

Datum int4_fixeddecimal_lt(PG_FUNCTION_ARGS) {
    int128 val1 = PG_GETARG_INT32(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val2sign = 1;

    if (ISSet(val2->sign)) {
        val2sign = -1;
    }

    multiplier = get_multiplier(val2->scale);
    val1 = val1 * multiplier;
    arg = val2sign * (val2->value);
    PG_RETURN_BOOL(val1 < arg);
}

Datum int4_fixeddecimal_gt(PG_FUNCTION_ARGS) {
    int128 val1 = PG_GETARG_INT32(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val2sign = 1;

    if (ISSet(val2->sign)) {
        val2sign = -1;
    }

    multiplier = get_multiplier(val2->scale);
    val1 = val1 * multiplier;
    arg = val2sign * (val2->value);
    PG_RETURN_BOOL(val1 > arg);
}

Datum int4_fixeddecimal_le(PG_FUNCTION_ARGS) {
    int128 val1 = PG_GETARG_INT32(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val2sign = 1;

    if (ISSet(val2->sign)) {
        val2sign = -1;
    }

    multiplier = get_multiplier(val2->scale);
    val1 = val1 * multiplier;
    arg = val2sign * (val2->value);
    PG_RETURN_BOOL(val1 <= arg);
}

Datum int4_fixeddecimal_ge(PG_FUNCTION_ARGS) {
    int128 val1 = PG_GETARG_INT32(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int128 multiplier = 1;
    int128 arg = 0;
    int val2sign = 1;

    if (ISSet(val2->sign)) {
        val2sign = -1;
    }

    multiplier = get_multiplier(val2->scale);
    val1 = val1 * multiplier;
    arg = val2sign * (val2->value);
    PG_RETURN_BOOL(val1 >= arg);
}

Datum int4_fixeddecimal_cmp(PG_FUNCTION_ARGS) {
    int128 val1 = PG_GETARG_INT32(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int val2sign = 1;
    int128 multiplier = 1;
    int128 arg = val1;

    if (ISSet(val2->sign)) {
        val2sign = -1;
    }

    multiplier = get_multiplier(val2->scale);
    val1 = val1 * multiplier;
    arg = val2sign * (val2->value);
    if (val1 == arg)
        PG_RETURN_INT32(0);
    else if (val1 < arg)
        PG_RETURN_INT32(-1);
    else
        PG_RETURN_INT32(1);
}

/* fixeddecimal, numeric */
Datum fixeddecimal_numeric_cmp(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    Numeric val2 = PG_GETARG_NUMERIC(1);
    Datum arg2;
    arg2 = DirectFunctionCall2(numeric_fixeddecimal, NumericGetDatum(val2), -1);

    PG_RETURN_INT32(DirectFunctionCall2(fixeddecimal_cmp, FixedDecimalGetDatum(arg1), arg2));
}

Datum fixeddecimal_numeric_eq(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    Datum val2 = PG_GETARG_DATUM(1);
    int32 result;

    result = DatumGetInt32(DirectFunctionCall2(fixeddecimal_numeric_cmp, FixedDecimalGetDatum(val1), val2));

    PG_RETURN_BOOL(result == 0);
}

Datum fixeddecimal_numeric_ne(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    Datum val2 = PG_GETARG_DATUM(1);
    int32 result;

    result = DatumGetInt32(DirectFunctionCall2(fixeddecimal_numeric_cmp, FixedDecimalGetDatum(val1), val2));

    PG_RETURN_BOOL(result != 0);
}

Datum fixeddecimal_numeric_lt(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    Datum val2 = PG_GETARG_DATUM(1);
    int32 result;

    result = DatumGetInt32(DirectFunctionCall2(fixeddecimal_numeric_cmp, FixedDecimalGetDatum(val1), val2));

    PG_RETURN_BOOL(result < 0);
}

Datum fixeddecimal_numeric_gt(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    Datum val2 = PG_GETARG_DATUM(1);
    int32 result;

    result = DatumGetInt32(DirectFunctionCall2(fixeddecimal_numeric_cmp, FixedDecimalGetDatum(val1), val2));

    PG_RETURN_BOOL(result > 0);
}

Datum fixeddecimal_numeric_le(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    Datum val2 = PG_GETARG_DATUM(1);
    int32 result;

    result = DatumGetInt32(DirectFunctionCall2(fixeddecimal_numeric_cmp, FixedDecimalGetDatum(val1), val2));

    PG_RETURN_BOOL(result <= 0);
}

Datum fixeddecimal_numeric_ge(PG_FUNCTION_ARGS) {
    FixedDecimal *val1 = PG_GETARG_FIXDECTYPE(0);
    Datum val2 = PG_GETARG_DATUM(1);
    int32 result;

    result = DatumGetInt32(DirectFunctionCall2(fixeddecimal_numeric_cmp, FixedDecimalGetDatum(val1), val2));

    PG_RETURN_BOOL(result >= 0);
}

Datum numeric_fixeddecimal_cmp(PG_FUNCTION_ARGS) {
    Numeric val = PG_GETARG_NUMERIC(0);
    FixedDecimal *arg2 = PG_GETARG_FIXDECTYPE(1);
    Datum arg1;

    arg1 = DirectFunctionCall2(numeric_fixeddecimal, NumericGetDatum(val), -1);

    PG_RETURN_INT32(DirectFunctionCall2(fixeddecimal_cmp, arg1, FixedDecimalGetDatum(arg2)));
}

Datum numeric_fixeddecimal_eq(PG_FUNCTION_ARGS) {
    Numeric val1 = PG_GETARG_NUMERIC(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int32 result;

    result =
        DatumGetInt32(DirectFunctionCall2(numeric_fixeddecimal_cmp, NumericGetDatum(val1), FixedDecimalGetDatum(val2)));

    PG_RETURN_BOOL(result == 0);
}

Datum numeric_fixeddecimal_ne(PG_FUNCTION_ARGS) {
    Datum val1 = PG_GETARG_DATUM(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int32 result;

    result = DatumGetInt32(DirectFunctionCall2(numeric_fixeddecimal_cmp, val1, FixedDecimalGetDatum(val2)));

    PG_RETURN_BOOL(result != 0);
}

Datum numeric_fixeddecimal_lt(PG_FUNCTION_ARGS) {
    Datum val1 = PG_GETARG_DATUM(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int32 result;

    result = DatumGetInt32(DirectFunctionCall2(numeric_fixeddecimal_cmp, val1, FixedDecimalGetDatum(val2)));

    PG_RETURN_BOOL(result < 0);
}

Datum numeric_fixeddecimal_gt(PG_FUNCTION_ARGS) {
    Datum val1 = PG_GETARG_DATUM(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int32 result;

    result = DatumGetInt32(DirectFunctionCall2(numeric_fixeddecimal_cmp, val1, FixedDecimalGetDatum(val2)));

    PG_RETURN_BOOL(result > 0);
}

Datum numeric_fixeddecimal_le(PG_FUNCTION_ARGS) {
    Datum val1 = PG_GETARG_DATUM(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int32 result;

    result = DatumGetInt32(DirectFunctionCall2(numeric_fixeddecimal_cmp, val1, FixedDecimalGetDatum(val2)));

    PG_RETURN_BOOL(result <= 0);
}

Datum numeric_fixeddecimal_ge(PG_FUNCTION_ARGS) {
    Datum val1 = PG_GETARG_DATUM(0);
    FixedDecimal *val2 = PG_GETARG_FIXDECTYPE(1);
    int32 result;

    result = DatumGetInt32(DirectFunctionCall2(numeric_fixeddecimal_cmp, val1, FixedDecimalGetDatum(val2)));

    PG_RETURN_BOOL(result >= 0);
}

/*----------------------------------------------------------
 *	Conversion operators.
 *---------------------------------------------------------*/

Datum int4fixeddecimal(PG_FUNCTION_ARGS) {
    int128 arg = PG_GETARG_INT64(0);
    FixedDecimal *val;
    val = (FixedDecimal *)palloc(MAXFIXALIGN);
    val->precision = 0;
    val->scale = 0;
    if (arg < 0) {
        val->sign = -1;
        val->value = (-1) * (arg);
    } else {
        val->sign = 1;
        val->value = arg;
    }

    while (arg > 0) {
        val->precision = val->precision + 1;
        arg = arg / 10;
    }

    PG_RETURN_FIXDECTYPE(val);
}

Datum int2fixeddecimal(PG_FUNCTION_ARGS) {
    int128 arg = PG_GETARG_INT16(0);
    FixedDecimal *val;
    val = (FixedDecimal *)palloc(MAXFIXALIGN);
    val->precision = 0;
    val->scale = 0;
    if (arg < 0) {
        val->sign = -1;
        val->value = (-1) * (arg);
    } else {
        val->sign = 1;
        val->value = arg;
    }

    while (arg > 0) {
        val->precision = val->precision + 1;
        arg = arg / 10;
    }

    PG_RETURN_FIXDECTYPE(val);
}

Datum dtofixeddecimal(PG_FUNCTION_ARGS) {
    float8 num = PG_GETARG_FLOAT8(0);
    FixedDecimal *result;
    char *str;
    char buffer[24 + 1];
    result = (FixedDecimal *)palloc(MAXFIXALIGN);
    sprintf(buffer, "%f", num);
    str = buffer;
    scan_arg_to_fixeddecimal(str, result);
    PG_RETURN_FIXDECTYPE(result);
}

Datum ftofixeddecimal(PG_FUNCTION_ARGS) {
    float4 num = PG_GETARG_FLOAT4(0);
    FixedDecimal *result;
    char *str;
    char buffer[24 + 1];
    result = (FixedDecimal *)palloc(MAXFIXALIGN);
    sprintf(buffer, "%f", num);
    str = buffer;
    scan_arg_to_fixeddecimal(str, result);
    PG_RETURN_FIXDECTYPE(result);
}

Datum numeric_fixeddecimal(PG_FUNCTION_ARGS) {
    Datum num;
    char *tmp;
    int32 typmod;
    FixedDecimal *result;
    num = PG_GETARG_DATUM(0);
    typmod = PG_GETARG_INT32(1);
    tmp = DatumGetCString(DirectFunctionCall1(numeric_out, num));
    num = DirectFunctionCall3(fixeddecimalin, CStringGetDatum(tmp), 0, typmod);
    result = (FixedDecimal *)palloc(MAXFIXALIGN);
    result = DatumGetFixedDecimal(num);
    pfree(tmp);

    PG_RETURN_FIXDECTYPE(result);
}

Datum fixeddecimalint4(PG_FUNCTION_ARGS) {
    FixedDecimal *arg = PG_GETARG_FIXDECTYPE(0);
    int128 num = arg->value;

    if (ISSet(arg->sign)) {
        num = -1 * (arg->value);
    }

    if ((int32)num != num)
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("integer out of range")));

    PG_RETURN_INT32((int32)num);
}

Datum fixeddecimalint2(PG_FUNCTION_ARGS) {
    FixedDecimal *arg = PG_GETARG_FIXDECTYPE(0);
    int128 num = arg->value;

    if (ISSet(arg->sign)) {
        num = -1 * (arg->value);
    }

    if ((int16)num != num)
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("integer out of range")));

    PG_RETURN_INT16((int16)num);
}

Datum fixeddecimaltod(PG_FUNCTION_ARGS) {
    FixedDecimal *arg = PG_GETARG_FIXDECTYPE(0);
    float8 result;
    int128 multiplier = 1;
    int scale;
    scale = arg->scale;

    while (scale > 0) {
        multiplier *= 10;
        scale--;
    }

    result = (float8)arg->value / multiplier;
    result = (arg->sign) * result;
    PG_RETURN_FLOAT8(result);
}

Datum fixeddecimaltof(PG_FUNCTION_ARGS) {
    FixedDecimal *arg = PG_GETARG_FIXDECTYPE(0);
    float4 result;
    int128 multiplier = 1;
    int scale;
    scale = arg->scale;

    while (scale > 0) {
        multiplier *= 10;
        scale--;
    }

    result = (float4)arg->value / multiplier;
    result = (arg->sign) * result;
    PG_RETURN_FLOAT4(result);
}

Datum fixeddecimal_numeric(PG_FUNCTION_ARGS) {
    FixedDecimal *num = PG_GETARG_FIXDECTYPE(0);
    char *tmp;
    Datum result;

    tmp = DatumGetCString(DirectFunctionCall1(fixeddecimalout, FixedDecimalGetDatum(num)));

    result = DirectFunctionCall3(numeric_in, CStringGetDatum(tmp), 0, -1);
    pfree(tmp);
    PG_RETURN_DATUM(result);
}

/*
 * fixeddecimal serves as casting function for fixeddecimal to fixeddecimal.
 * The only serves to generate an error if the fixedecimal is too big for the
 * specified typmod.
 */
Datum fixeddecimal(PG_FUNCTION_ARGS) {
    FixedDecimal *num = PG_GETARG_FIXDECTYPE(0);
    int32 typmod = PG_GETARG_INT32(1);
    FixedDecimal *result;
    Datum val = 0;

    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    /* no need to check typmod if it's -1 */
    if (typmod != -1) {
        val = DirectFunctionCall1(fixeddecimalout, FixedDecimalGetDatum(num));
        val = DirectFunctionCall3(fixeddecimalin, val, 0, typmod);
    }
    result = DatumGetFixedDecimal(val);
    PG_RETURN_FIXDECTYPE(result);
}

static void scan_arg_to_fixeddecimal(char *cp, FixedDecimal *arg) {
    int128 integralpart = 0;
    int128 fractionalpart = 0;
    int128 value = 0;
    int precision = 0;
    int scale = 0;
    int sign = 1;

    /*Skip leading zeroes*/
    while (*cp) {
        if (!isspace((unsigned char)*cp))
            break;
        cp++;
    }

    /*Special value checks*/
    if (pg_strncasecmp(cp, "NaN", 3) == 0) {
        sign = 4;
    } else if (pg_strncasecmp(cp, "Infinity", 8) == 0) {
        sign = 2;
    } else if (pg_strncasecmp(cp, "+Infinity", 9) == 0) {
        sign = 2;
    } else if (pg_strncasecmp(cp, "-Infinity", 9) == 0) {
        sign = 3;
    } else if (pg_strncasecmp(cp, "inf", 3) == 0) {
        sign = 2;
    } else if (pg_strncasecmp(cp, "+inf", 4) == 0) {
        sign = 2;
    } else if (pg_strncasecmp(cp, "-inf", 4) == 0) {
        sign = 3;
    } else {
        int128 multiplier = 1;

        if (*cp == '-') {
            sign = -1;
            cp++;
        }

        if (*cp == '+') {
            cp++;
        }

        while (isdigit((unsigned char)*cp)) {
            int128 tmp = integralpart * 10 + (*cp++ - '0');
            precision++;
            integralpart = tmp;
        }

        value = integralpart;

        if (*cp == '.') {
            cp++;
        }

        while (isdigit((unsigned char)*cp)) {
            int128 tmp = fractionalpart * 10 + (*cp++ - '0');
            precision++;
            scale++;
            multiplier = multiplier * 10;
            fractionalpart = tmp;
        }

        Assert(precision <= MAX_PRECISION);

        value = value * multiplier + fractionalpart;
        arg->precision = precision;
        arg->scale = scale;
        arg->sign = sign;
        arg->value = value;
    }
}

/*----------------------------------------------------------
 *	Arithmetic operators on fixeddecimal.

    1. Addition Operations :
        * Check whether inputs are special values.
        * Get scale difference and multiplier.
        * Based on higher scale multiplier in multiplied to smaller number.
        * Return fixeddecimal result.

    2. Subtraction Operation :
        * Check whether inputs are special values.
        * Get scale difference and multiplier.
        * Based on higher scale multiplier in multiplied to smaller number.
        * Return fixeddecimal result.

    3. Multiplication Operation :
        * Check whether inputs are special values.
        * Add scale of both numbers to get resultant scale.
        * Return fixeddecimal result.

    4. Division Operation :
        * Check whether inputs are special values.
        * Get scale difference and multiplier.
        * Based on higher scale multiplier in multiplied to smaller number.
        * Return fixeddecimal result.
 *---------------------------------------------------------*/

Datum fixeddecimalum(PG_FUNCTION_ARGS) {
    FixedDecimal *arg = PG_GETARG_FIXDECTYPE(0);
    int128 result;

#ifdef HAVE_BUILTIN_OVERFLOW
    int128 zero = 0;

    if (__builtin_sub_overflow(zero, arg, &result))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));
#else
    if (ISSet(arg->sign)) {
        result = arg->value;
    } else {
        result = -(arg->value);
    }
    /* overflow check (needed for INT64_MIN) */
    if (arg != 0 && SAMESIGN(result, (arg->value) * (arg->sign)))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));
#endif /* HAVE_BUILTIN_OVERFLOW */
    PG_RETURN_INT64(result);
}

Datum fixeddecimalup(PG_FUNCTION_ARGS) {
    FixedDecimal *arg = PG_GETARG_FIXDECTYPE(0);
    if (ISSet(arg->sign)) {
        PG_RETURN_INT64((-1) * (arg->value));
    }
    PG_RETURN_INT64((arg->value) * (arg->sign));
}

Datum fixeddecimalpl(PG_FUNCTION_ARGS) {
    FixedDecimal *val1;
    FixedDecimal *val2;
    int128 arg1;
    int128 arg2;
    int arg1sign = 1;
    int arg2sign = 1;
    val1 = PG_GETARG_FIXDECTYPE(0);
    val2 = PG_GETARG_FIXDECTYPE(1);
    arg1 = val1->value;
    arg2 = val2->value;

    if (ISSet(val1->sign)) {
        arg1sign = -1;
    }

    if (ISSet(val2->sign)) {
        arg2sign = -1;
    }

    arg1 = arg1sign * arg1;
    arg2 = arg2sign * arg2;

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_add_overflow(arg1, arg2, &arg1))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#else
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(val1->sign) || ISSpecial(val2->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(val1->sign) || ISNaN(val2->sign))
            result->sign = 4;
        if (ISPinf(val1->sign)) {
            if (ISNinf(val2->sign))
                result->sign = 4; /* Inf + -Inf */
            else
                result->sign = 2;
        }
        if (ISNinf(val1->sign)) {
            if (ISPinf(val2->sign))
                result->sign = 4; /* -Inf + Inf */
            else
                result->sign = 3;
            ;
        }
        /* by here, num1 must be finite, so num2 is not */
        if (ISPinf(val2->sign)) {
            result->sign = 2;
        } else {
            result->sign = 3;
        }
    } else {
        int128 temp = 0;
        int128 multiplier;
        uint8 scale_difference = 0;
        uint8 precision = 0;
        scale_difference = get_scale_difference(val1->scale, val2->scale);
        multiplier = get_multiplier(scale_difference);

        if ((val1->scale) >= (val2->scale)) {
            result->scale = val1->scale;
            arg2 = arg2 * multiplier;
        } else {
            result->scale = val2->scale;
            arg1 = arg1 * multiplier;
        }

        temp = arg1 + arg2;

        if (temp < 0) {
            result->sign = -1;
            result->value = -1 * temp;
            temp = -1 * temp;
        } else {
            result->sign = 1;
            result->value = temp;
        }

        precision = get_precision(temp);
        result->precision = precision;
    }

    if (SAMESIGN(arg1, arg2) && !SAMESIGN(result->value * result->sign, arg1))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#endif
    PG_RETURN_FIXDECTYPE(result);
}

Datum fixeddecimalmi(PG_FUNCTION_ARGS) {
    FixedDecimal *val1;
    FixedDecimal *val2;
    int128 arg1;
    int128 arg2;
    int arg1sign = 1;
    int arg2sign = 1;
    val1 = PG_GETARG_FIXDECTYPE(0);
    val2 = PG_GETARG_FIXDECTYPE(1);
    arg1 = val1->value;
    arg2 = val2->value;

    if (ISSet(val1->sign)) {
        arg1sign = -1;
    }

    if (ISSet(val2->sign)) {
        arg2sign = -1;
    }

    arg1 = arg1sign * arg1;
    arg2 = arg2sign * arg2;

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_sub_overflow(arg1, arg2, &arg1))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#else
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(val1->sign) || ISSpecial(val2->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;
        if (ISNaN(val1->sign) || ISNaN(val2->sign))
            result->sign = 4;
        if (ISPinf(val1->sign)) {
            if (ISPinf(val2->sign))
                result->sign = 4; /* Inf - Inf */
            else
                result->sign = 2;
        }
        if (ISNinf(val1->sign)) {
            if (ISNinf(val2->sign))
                result->sign = 4; /* -Inf - -Inf */
            else
                result->sign = 3;
        }
        /* by here, num1 must be finite, so num2 is not */
        if (ISPinf(val2->sign))
            result->sign = 3;
        result->sign = 2;
    } else {
        int128 temp = 0;
        int128 multiplier;
        uint8 scale_difference = 0;
        uint8 precision = 0;
        scale_difference = get_scale_difference(val1->scale, val2->scale);
        multiplier = get_multiplier(scale_difference);

        if ((val1->scale) >= (val2->scale)) {
            result->scale = val1->scale;
            arg2 = arg2 * multiplier;
        } else {
            result->scale = val2->scale;
            arg1 = arg1 * multiplier;
        }

        temp = arg1 - arg2;

        if (temp < 0) {
            result->sign = -1;
            result->value = -1 * temp;
            temp = -1 * temp;
        } else {
            result->sign = 1;
            result->value = temp;
        }

        precision = get_precision(temp);
        result->precision = precision;
    }
#endif
    PG_RETURN_FIXDECTYPE(result);
}

Datum fixeddecimalmul(PG_FUNCTION_ARGS) {
    FixedDecimal *val1;
    FixedDecimal *val2;

    int128 arg1;
    int128 arg2;
    int arg1sign = 1;
    int arg2sign = 1;
    val1 = PG_GETARG_FIXDECTYPE(0);
    val2 = PG_GETARG_FIXDECTYPE(1);
    arg1 = val1->value;
    arg2 = val2->value;

    if (ISSet(val1->sign)) {
        arg1sign = -1;
    }

    if (ISSet(val2->sign)) {
        arg2sign = -1;
    }

    arg1 = arg1sign * arg1;
    arg2 = arg2sign * arg2;

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_mul_overflow(arg1, arg2, &arg1))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#else
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(val1->sign) || ISSpecial(val2->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;
        if (ISNaN(val1->sign) || ISNaN(val2->sign))
            result->sign = 4;
        if (ISPinf(val1->sign)) {
            if (ISPinf(val2->sign))
                result->sign = 2; /* Inf - Inf */
            else if (ISNinf(val2->sign)) {
                result->sign = 3;
            } else {
                result->sign = 4;
            }
        } else if (ISNinf(val1->sign)) {
            if (ISPinf(val2->sign))
                result->sign = 2; /* Inf - Inf */
            else if (ISNinf(val2->sign)) {
                result->sign = 3;
            } else {
                result->sign = 4;
            }
        } else {
            if (ISPinf(val2->sign))
                result->sign = 2; /* Inf - Inf */
            else if (ISNinf(val2->sign)) {
                result->sign = 3;
            } else {
                result->sign = 4;
            }
        }
    } else {
        int128 temp = 0;
        uint8 precision = 0;
        result->scale = val1->scale + val2->scale;
        temp = arg1 * arg2;

        if (temp < 0) {
            result->sign = -1;
            result->value = -1 * temp;
            temp = -1 * temp;
        } else {
            result->sign = 1;
            result->value = temp;
        }

        precision = get_precision(temp);
        result->precision = precision;
    }
#endif
    PG_RETURN_FIXDECTYPE(result);
}

Datum fixeddecimaldiv(PG_FUNCTION_ARGS) {
    FixedDecimal *val1;
    FixedDecimal *val2;
    FixedDecimal *result;
    int128 arg1;
    int128 arg2;
    val1 = PG_GETARG_FIXDECTYPE(0);
    val2 = PG_GETARG_FIXDECTYPE(1);
    arg1 = val1->value;
    arg2 = val2->value;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(val1->sign) || ISSpecial(val2->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(val1->sign) || ISNaN(val2->sign))
            result->sign = 4;

        if (ISPinf(val1->sign)) {
            if (ISSpecial(val2->sign)) {
                result->sign = 4; /* Inf / [-]Inf */
            } else if (val2->value == 0) {
                ereport(ERROR, (errcode(ERRCODE_DIVISION_BY_ZERO), errmsg("division by zero")));
            } else {
                result->sign = 4;
            }
        } else if (ISNinf(val1->sign)) {
            if (ISSpecial(val2->sign)) {
                result->sign = 4; /* -Inf / [-]Inf */
            } else if (val2->value == 0) {
                ereport(ERROR, (errcode(ERRCODE_DIVISION_BY_ZERO), errmsg("division by zero")));
            } else {
                result->sign = 4;
            }
        } else {
            if (val1->value == 0) {
                result->sign = 1;
            } else {
                if (ISSpecial(val2->sign)) {
                    result->sign = 4; /* -Inf / [-]Inf */
                }

                if (val2->value == 0) {
                    ereport(ERROR, (errcode(ERRCODE_DIVISION_BY_ZERO), errmsg("division by zero")));
                }
            }
        }
    } else {
        int128 temp = 0;
        int128 multiplier = 1;
        int precision = 0;
        int scale_difference = 0;
        if (arg2 == 0) {
            ereport(ERROR, (errcode(ERRCODE_DIVISION_BY_ZERO), errmsg("division by zero")));
            /* ensure compiler realizes we mustn't reach the division (gcc bug) */
            PG_RETURN_NULL();
        }

        if (arg2 == 0) {
            ereport(ERROR, (errcode(ERRCODE_DIVISION_BY_ZERO), errmsg("division by zero")));
        }

        scale_difference = get_scale_difference(val1->scale, val2->scale);
        result->scale = scale_difference;
        multiplier = get_multiplier(result->scale);

        if (ISSet(val1->sign)) {
            arg1 = -1 * arg1;
        }

        if (ISSet(val2->sign)) {
            arg2 = -1 * arg2;
        }

        if (val1->scale < val2->scale) {
            arg1 = arg1 * multiplier * multiplier;
        }

        temp = arg1 / arg2;

        if (temp < 0) {
            result->sign = -1;
            result->value = -1 * temp;
            temp = -1 * temp;
        } else {
            result->sign = 1;
            result->value = temp;
        }

        precision = get_precision(temp);

        if (precision > MAX_PRECISION) {
            ereport(ERROR, errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("Value out of range"));
        }

        result->precision = precision;
    }
    PG_RETURN_FIXDECTYPE(result);
}

/* fixeddecimalabs()
 * Absolute value
 */
Datum fixeddecimalabs(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    if (ISSet(arg1->sign)) {
        arg1->sign = 1;
    }

    PG_RETURN_FIXDECTYPE(arg1);
}

Datum fixeddecimallarger(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    FixedDecimal *arg2 = PG_GETARG_FIXDECTYPE(1);

    if (ISSet(arg1->sign) && !ISSet(arg2->sign)) {
        PG_RETURN_FIXDECTYPE(arg2);
    } else if (!ISSet(arg1->sign) && ISSet(arg2->sign)) {
        PG_RETURN_FIXDECTYPE(arg1);
    } else {
        if (arg1->value > arg2->value) {
            PG_RETURN_FIXDECTYPE(arg1);
        } else {
            PG_RETURN_FIXDECTYPE(arg2);
        }
    }
}

Datum fixeddecimalsmaller(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    FixedDecimal *arg2 = PG_GETARG_FIXDECTYPE(1);

    if (ISSet(arg1->sign) && !ISSet(arg2->sign)) {
        PG_RETURN_FIXDECTYPE(arg1);
    } else if (!ISSet(arg1->sign) && ISSet(arg2->sign)) {
        PG_RETURN_FIXDECTYPE(arg2);
    } else {
        if (arg1->value > arg2->value) {
            PG_RETURN_FIXDECTYPE(arg2);
        } else {
            PG_RETURN_FIXDECTYPE(arg1);
        }
    }
}

Datum fixeddecimalint4pl(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    int128 adder = PG_GETARG_INT32(1);
    int arg1sign = 1;
    int128 add_result = 0;

    if (ISSet(arg1->sign)) {
        arg1sign = -1;
    }

    add_result = arg1sign * arg1->value;

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_add_overflow(add_result, adder, &add_result))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#else
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(arg1->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg1->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg1->sign)) {
            result->sign = 2;
        }

        if (ISNinf(arg1->sign)) {
            result->sign = 3;
        }
    } else {
        uint128 multiplier;
        result->scale = arg1->scale;
        multiplier = get_multiplier(result->scale);
        adder = adder * multiplier;
        add_result = add_result + adder;

        if (add_result < 0) {
            result->sign = -1;
            result->value = -1 * add_result;
        } else {
            result->sign = 1;
            result->value = add_result;
        }

        result->precision = get_precision(result->value);
    }
#endif
    PG_RETURN_FIXDECTYPE(result);
}

Datum fixeddecimalint4mi(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    int128 subtractor = PG_GETARG_INT32(1);
    int arg1sign = 1;
    int128 sub_result = 0;

    if (ISSet(arg1->sign)) {
        arg1sign = -1;
    }

    sub_result = arg1sign * arg1->value;

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_sub_overflow(sub_result, subtractor, &sub_result))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#else
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(arg1->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg1->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg1->sign)) {
            result->sign = 2;
        }

        if (ISNinf(arg1->sign)) {
            result->sign = 3;
        }
    } else {
        uint128 multiplier;
        result->scale = arg1->scale;
        multiplier = get_multiplier(result->scale);
        subtractor = subtractor * multiplier;
        sub_result = sub_result - subtractor;
        if (sub_result < 0) {
            result->sign = -1;
            result->value = -1 * sub_result;
        } else {
            result->sign = 1;
            result->value = sub_result;
        }

        result->precision = get_precision(result->value);
    }
#endif
    PG_RETURN_FIXDECTYPE(result);
}

Datum fixeddecimalint4mul(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    int32 arg2 = PG_GETARG_INT32(1);
    int arg1sign = 1;
    int128 mul_result = 0;

    if (ISSet(arg1->sign)) {
        arg1sign = -1;
    }

    mul_result = arg1sign * arg1->value;

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_mul_overflow(mul_result, arg2, &mul_result))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#else
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(arg1->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg1->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg1->sign)) {
            result->sign = 2;
        }

        if (ISNinf(arg1->sign)) {
            result->sign = 3;
        }
    } else {
        result->scale = arg1->scale;
        mul_result = mul_result * arg2;

        if (mul_result < 0) {
            result->sign = -1;
            result->value = -1 * mul_result;
        } else {
            result->sign = 1;
            result->value = mul_result;
        }

        result->precision = get_precision(result->value);
    }
#endif
    PG_RETURN_FIXDECTYPE(result);
}

Datum fixeddecimalint4div(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    int32 arg2 = PG_GETARG_INT32(1);
    FixedDecimal *result;
    int arg1sign = 1;
    int128 div_result = 0;

    if (arg2 == 0) {
        ereport(ERROR, (errcode(ERRCODE_DIVISION_BY_ZERO), errmsg("division by zero")));
        /* ensure compiler realizes we mustn't reach the division (gcc bug) */
        PG_RETURN_NULL();
    }

    if (arg2 == 0) {
        ereport(ERROR, (errcode(ERRCODE_DIVISION_BY_ZERO), errmsg("division by zero")));
    }

    if (ISSet(arg1->sign)) {
        arg1sign = -1;
    }

    result = (FixedDecimal *)palloc(MAXFIXALIGN);
    result->scale = arg1->scale;

    if (ISSpecial(arg1->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg1->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg1->sign)) {
            result->sign = 2;
        }

        if (ISNinf(arg1->sign)) {
            result->sign = 3;
        }
    } else {
        div_result = arg1sign * arg1->value;
        div_result = div_result / arg2;

        if (div_result < 0) {
            result->sign = -1;
            result->value = -1 * div_result;
        } else {
            result->sign = 1;
            result->value = div_result;
        }

        result->precision = get_precision(result->value);
    }
    PG_RETURN_FIXDECTYPE(result);
}

Datum int4fixeddecimalpl(PG_FUNCTION_ARGS) {
    int128 adder = PG_GETARG_INT32(0);
    FixedDecimal *arg2 = PG_GETARG_FIXDECTYPE(1);
    int arg2sign = 1;
    int128 add_result = 0;

    if (ISSet(arg2->sign)) {
        arg2sign = -1;
    }

    add_result = (arg2sign) * (arg2->value);

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_add_overflow(adder, add_result, &add_result))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#else
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(arg2->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg2->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg2->sign)) {
            result->sign = 2;
        }

        if (ISNinf(arg2->sign)) {
            result->sign = 3;
        }
    } else {
        uint128 multiplier;
        multiplier = get_multiplier(arg2->scale);
        adder = adder * multiplier;
        result->scale = arg2->scale;
        add_result = adder + add_result;
        if (add_result < 0) {
            result->sign = -1;
            result->value = -1 * add_result;
        } else {
            result->sign = 1;
            result->value = add_result;
        }

        result->precision = get_precision(result->value);
    }
#endif
    PG_RETURN_FIXDECTYPE(result);
}

Datum int4fixeddecimalmi(PG_FUNCTION_ARGS) {
    int128 subtractor = PG_GETARG_INT32(0);
    FixedDecimal *arg2 = PG_GETARG_FIXDECTYPE(1);
    int arg2sign = 1;
    int128 sub_result = 0;

    if (ISSet(arg2->sign)) {
        arg2sign = -1;
    }

    sub_result = (arg2sign) * (arg2->value);

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_sub_overflow(subtractor, sub_result, &sub_result))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#else
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(arg2->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg2->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg2->sign)) {
            result->sign = 3;
        }

        if (ISNinf(arg2->sign)) {
            result->sign = 2;
        }
    } else {
        uint128 multiplier;
        multiplier = get_multiplier(arg2->scale);
        subtractor = subtractor * multiplier;
        result->scale = arg2->scale;
        sub_result = subtractor - sub_result;
        if (sub_result < 0) {
            result->value = -1 * sub_result;
            result->sign = -1;
        } else {
            result->sign = 1;
            result->value = sub_result;
        }

        result->precision = get_precision(result->value);
    }
#endif
    PG_RETURN_FIXDECTYPE(result);
}

Datum int4fixeddecimalmul(PG_FUNCTION_ARGS) {
    int128 arg1 = PG_GETARG_INT32(0);
    FixedDecimal *arg2 = PG_GETARG_FIXDECTYPE(1);
    int arg2sign = 1;
    int128 mul_result = 0;

    if (ISSet(arg2->sign)) {
        arg2sign = -1;
    }

    mul_result = arg2sign * arg2->value;

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_mul_overflow(arg1, mul_result, &mul_result))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#else
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(arg2->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg2->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg2->sign)) {
            result->sign = 2;
        }

        if (ISNinf(arg2->sign)) {
            result->sign = 3;
        }
    } else {
        result->scale = arg2->scale;
        mul_result = mul_result * arg1;

        if (mul_result < 0) {
            result->sign = -1;
            result->value = -1 * mul_result;
        } else {
            result->sign = 1;
            result->value = mul_result;
        }

        result->precision = get_precision(result->value);
    }
#endif
    PG_RETURN_FIXDECTYPE(result);
}

Datum int4fixeddecimaldiv(PG_FUNCTION_ARGS) {
    int128 arg1 = PG_GETARG_INT32(0);
    FixedDecimal *arg2 = PG_GETARG_FIXDECTYPE(1);
    int arg2sign = 1;
    int128 multiplier = 1;
    float128 div_result = 0;

    if (ISSet(arg2->sign)) {
        arg2sign = -1;
    }

    div_result = (float128)arg2sign * arg2->value;

    if (div_result == 0) {
        ereport(ERROR, (errcode(ERRCODE_DIVISION_BY_ZERO), errmsg("division by zero")));
        /* ensure compiler realizes we mustn't reach the division (gcc bug) */
        PG_RETURN_NULL();
    }

    if (div_result == 0) {
        ereport(ERROR, (errcode(ERRCODE_DIVISION_BY_ZERO), errmsg("division by zero")));
    }

    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(arg2->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg2->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg2->sign)) {
            result->sign = 1;
        }

        if (ISNinf(arg2->sign)) {
            result->sign = 1;
        }
    } else {
        result->scale = arg2->scale;
        multiplier = get_multiplier(arg2->scale);
        div_result = arg1 / div_result;
        div_result = multiplier * div_result;

        if (div_result < 0) {
            result->sign = -1;
            result->value = (int128)(-1 * div_result * multiplier);
        } else {
            result->sign = 1;
            result->value = (int128)(div_result * multiplier);
        }

        result->precision = get_precision(result->value);
    }

    PG_RETURN_FIXDECTYPE(result);
}

Datum fixeddecimalint2pl(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    int128 adder = PG_GETARG_INT16(1);
    int arg1sign = 1;
    int128 add_result = 0;

    if (ISSet(arg1->sign)) {
        arg1sign = -1;
    }

    add_result = arg1sign * arg1->value;

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_add_overflow(add_result, adder, &add_result))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#else
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(arg1->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg1->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg1->sign)) {
            result->sign = 2;
        }

        if (ISNinf(arg1->sign)) {
            result->sign = 3;
        }
    } else {
        uint128 multiplier;
        result->scale = arg1->scale;
        multiplier = get_multiplier(result->scale);
        adder = adder * multiplier;
        add_result = add_result + adder;

        if (add_result < 0) {
            result->sign = -1;
            result->value = -1 * add_result;
        } else {
            result->sign = 1;
            result->value = add_result;
        }

        result->precision = get_precision(result->value);
    }
#endif
    PG_RETURN_FIXDECTYPE(result);
}

Datum fixeddecimalint2mi(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    int128 subtractor = PG_GETARG_INT16(1);
    int arg1sign = 1;
    int128 sub_result = 0;

    if (ISSet(arg1->sign)) {
        arg1sign = -1;
    }

    sub_result = arg1sign * arg1->value;

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_sub_overflow(sub_result, subtractor, &sub_result))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#else
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(arg1->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg1->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg1->sign)) {
            result->sign = 2;
        }

        if (ISNinf(arg1->sign)) {
            result->sign = 3;
        }
    } else {
        uint128 multiplier;
        result->scale = arg1->scale;
        multiplier = get_multiplier(result->scale);
        subtractor = subtractor * multiplier;
        sub_result = sub_result - subtractor;
        if (sub_result < 0) {
            result->sign = -1;
            result->value = -1 * sub_result;
        } else {
            result->sign = 1;
            result->value = sub_result;
        }

        result->precision = get_precision(result->value);
    }
#endif
    PG_RETURN_FIXDECTYPE(result);
}

Datum fixeddecimalint2mul(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    int32 arg2 = PG_GETARG_INT16(1);
    int arg1sign = 1;
    int128 mul_result = 0;

    if (ISSet(arg1->sign)) {
        arg1sign = -1;
    }

    mul_result = arg1sign * arg1->value;

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_mul_overflow(mul_result, arg2, &mul_result))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#else
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(arg1->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg1->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg1->sign)) {
            result->sign = 2;
        }

        if (ISNinf(arg1->sign)) {
            result->sign = 3;
        }
    } else {
        result->scale = arg1->scale;
        mul_result = mul_result * arg2;

        if (mul_result < 0) {
            result->sign = -1;
            result->value = -1 * mul_result;
        } else {
            result->sign = 1;
            result->value = mul_result;
        }

        result->precision = get_precision(result->value);
    }
#endif
    PG_RETURN_FIXDECTYPE(result);
}

Datum fixeddecimalint2div(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    int16 arg2 = PG_GETARG_INT16(1);
    int arg1sign = 1;
    int128 div_result = 0;

    if (arg2 == 0) {
        ereport(ERROR, (errcode(ERRCODE_DIVISION_BY_ZERO), errmsg("division by zero")));
        /* ensure compiler realizes we mustn't reach the division (gcc bug) */
        PG_RETURN_NULL();
    }

    if (arg2 == 0) {
        ereport(ERROR, (errcode(ERRCODE_DIVISION_BY_ZERO), errmsg("division by zero")));
    }

    if (ISSet(arg1->sign)) {
        arg1sign = -1;
    }

    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(arg1->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg1->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg1->sign)) {
            result->sign = 2;
        }

        if (ISNinf(arg1->sign)) {
            result->sign = 3;
        }
    } else {
        result->scale = arg1->scale;
        div_result = arg1sign * arg1->value;
        div_result = div_result / arg2;

        if (div_result < 0) {
            result->sign = -1;
            result->value = -1 * div_result;
        } else {
            result->sign = 1;
            result->value = div_result;
        }

        result->precision = get_precision(result->value);
    }

    PG_RETURN_FIXDECTYPE(result);
}

Datum int2fixeddecimalpl(PG_FUNCTION_ARGS) {
    int128 adder = PG_GETARG_INT16(0);
    FixedDecimal *arg2 = PG_GETARG_FIXDECTYPE(1);
    int arg2sign = 1;
    int128 add_result = 0;

    if (ISSet(arg2->sign)) {
        arg2sign = -1;
    }

    add_result = (arg2sign) * (arg2->value);

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_add_overflow(adder, add_result, &add_result))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#else
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(arg2->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg2->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg2->sign)) {
            result->sign = 2;
        }

        if (ISNinf(arg2->sign)) {
            result->sign = 3;
        }
    } else {
        uint128 multiplier;
        multiplier = get_multiplier(arg2->scale);
        adder = adder * multiplier;
        result->scale = arg2->scale;
        add_result = adder + add_result;
        if (add_result < 0) {
            result->sign = -1;
            result->value = -1 * add_result;
        } else {
            result->sign = 1;
            result->value = add_result;
        }

        result->precision = get_precision(result->value);
    }
#endif
    PG_RETURN_FIXDECTYPE(result);
}

Datum int2fixeddecimalmi(PG_FUNCTION_ARGS) {
    int128 subtractor = PG_GETARG_INT16(0);
    FixedDecimal *arg2 = PG_GETARG_FIXDECTYPE(1);
    int arg2sign = 1;
    int128 sub_result = 0;

    if (ISSet(arg2->sign)) {
        arg2sign = -1;
    }

    sub_result = (arg2sign) * (arg2->value);

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_sub_overflow(subtractor, sub_result, &sub_result))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#else
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(arg2->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg2->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg2->sign)) {
            result->sign = 3;
        }

        if (ISNinf(arg2->sign)) {
            result->sign = 2;
        }
    } else {
        uint128 multiplier;
        multiplier = get_multiplier(arg2->scale);
        subtractor = subtractor * multiplier;
        result->scale = arg2->scale;
        sub_result = subtractor - sub_result;
        if (sub_result < 0) {
            result->value = -1 * sub_result;
            result->sign = -1;
        } else {
            result->sign = 1;
            result->value = sub_result;
        }

        result->precision = get_precision(result->value);
    }
#endif
    PG_RETURN_FIXDECTYPE(result);
}

Datum int2fixeddecimalmul(PG_FUNCTION_ARGS) {
    int128 arg1 = PG_GETARG_INT16(0);
    FixedDecimal *arg2 = PG_GETARG_FIXDECTYPE(1);
    int arg2sign = 1;
    int128 mul_result = 0;

    if (ISSet(arg2->sign)) {
        arg2sign = -1;
    }

    mul_result = arg2sign * arg2->value;

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_mul_overflow(arg1, mul_result, &mul_result))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

#else
    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(arg2->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg2->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg2->sign)) {
            result->sign = 2;
        }

        if (ISNinf(arg2->sign)) {
            result->sign = 3;
        }
    } else {
        result->scale = arg2->scale;
        mul_result = mul_result * arg1;

        if (mul_result < 0) {
            result->sign = -1;
            result->value = -1 * mul_result;
        } else {
            result->sign = 1;
            result->value = mul_result;
        }

        result->precision = get_precision(result->value);
    }
#endif
    PG_RETURN_FIXDECTYPE(result);
}

Datum int2fixeddecimaldiv(PG_FUNCTION_ARGS) {
    int128 arg1 = PG_GETARG_INT16(0);
    FixedDecimal *arg2 = PG_GETARG_FIXDECTYPE(1);
    int arg2sign = 1;
    int128 multiplier = 1;
    float128 div_result = 0;

    if (ISSet(arg2->sign)) {
        arg2sign = -1;
    }

    div_result = (float128)arg2sign * arg2->value;

    if (div_result == 0) {
        ereport(ERROR, (errcode(ERRCODE_DIVISION_BY_ZERO), errmsg("division by zero")));
        /* ensure compiler realizes we mustn't reach the division (gcc bug) */
        PG_RETURN_NULL();
    }

    if (div_result == 0) {
        ereport(ERROR, (errcode(ERRCODE_DIVISION_BY_ZERO), errmsg("division by zero")));
    }

    FixedDecimal *result;
    result = (FixedDecimal *)palloc(MAXFIXALIGN);

    if (ISSpecial(arg2->sign)) {
        result->precision = MAX_PRECISION;
        result->scale = 0;
        result->value = 0;

        if (ISNaN(arg2->sign)) {
            result->sign = 4;
        }

        if (ISPinf(arg2->sign)) {
            result->sign = 1;
        }

        if (ISNinf(arg2->sign)) {
            result->sign = 1;
        }
    } else {
        result->scale = arg2->scale;
        multiplier = get_multiplier(arg2->scale);
        div_result = arg1 / div_result;
        div_result = multiplier * div_result;

        if (div_result < 0) {
            result->sign = -1;
            result->value = (int128)(-1 * div_result * multiplier);
        } else {
            result->sign = 1;
            result->value = (int128)(div_result * multiplier);
        }

        result->precision = get_precision(result->value);
    }

    PG_RETURN_FIXDECTYPE(result);
}

Datum fixeddecimalnumericpl(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    Numeric var = PG_GETARG_NUMERIC(1);
    Datum val;
    val = DirectFunctionCall2(numeric_fixeddecimal, NumericGetDatum(var), -1);
    val = DirectFunctionCall2(fixeddecimalpl, FixedDecimalGetDatum(arg1), val);
    PG_RETURN_DATUM(val);
}

Datum fixeddecimalnumericmi(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    Numeric var = PG_GETARG_NUMERIC(1);
    Datum val;
    val = DirectFunctionCall2(numeric_fixeddecimal, NumericGetDatum(var), -1);
    val = DirectFunctionCall2(fixeddecimalmi, FixedDecimalGetDatum(arg1), val);
    PG_RETURN_DATUM(val);
}

Datum fixeddecimalnumericmul(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    Numeric var = PG_GETARG_NUMERIC(1);
    Datum val;
    val = DirectFunctionCall2(numeric_fixeddecimal, NumericGetDatum(var), -1);
    val = DirectFunctionCall2(fixeddecimalmul, FixedDecimalGetDatum(arg1), val);
    PG_RETURN_DATUM(val);
}

Datum fixeddecimalnumericdiv(PG_FUNCTION_ARGS) {
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(0);
    Numeric var = PG_GETARG_NUMERIC(1);
    FixedDecimal *arg2;
    FixedDecimal *result;
    Datum val;
    val = DirectFunctionCall2(numeric_fixeddecimal, NumericGetDatum(var), -1);
    result = (FixedDecimal *)palloc(MAXFIXALIGN);
    arg2 = (FixedDecimal *)palloc(MAXFIXALIGN);
    arg2 = DatumGetFixedDecimal(val);

    val = DirectFunctionCall2(fixeddecimaldiv, FixedDecimalGetDatum(arg1), FixedDecimalGetDatum(arg2));
    result = DatumGetFixedDecimal(val);
    PG_RETURN_FIXDECTYPE(result);
}

Datum numericfixeddecimalpl(PG_FUNCTION_ARGS) {
    Numeric var = PG_GETARG_NUMERIC(0);
    FixedDecimal *arg2 = PG_GETARG_FIXDECTYPE(1);
    Datum val;
    val = DirectFunctionCall2(numeric_fixeddecimal, NumericGetDatum(var), -1);
    val = DirectFunctionCall2(fixeddecimalpl, val, FixedDecimalGetDatum(arg2));
    PG_RETURN_DATUM(val);
}

Datum numericfixeddecimalmi(PG_FUNCTION_ARGS) {
    Numeric var = PG_GETARG_NUMERIC(0);
    FixedDecimal *arg2 = PG_GETARG_FIXDECTYPE(1);
    Datum val;
    val = DirectFunctionCall2(numeric_fixeddecimal, NumericGetDatum(var), -1);
    val = DirectFunctionCall2(fixeddecimalmi, val, FixedDecimalGetDatum(arg2));
    PG_RETURN_DATUM(val);
}

Datum numericfixeddecimalmul(PG_FUNCTION_ARGS) {
    Numeric var = PG_GETARG_NUMERIC(0);
    FixedDecimal *arg2 = PG_GETARG_FIXDECTYPE(1);
    Datum val;
    val = DirectFunctionCall2(numeric_fixeddecimal, NumericGetDatum(var), -1);
    val = DirectFunctionCall2(fixeddecimalmul, val, FixedDecimalGetDatum(arg2));
    PG_RETURN_DATUM(val);
}

Datum numericfixeddecimaldiv(PG_FUNCTION_ARGS) {
    Numeric var = PG_GETARG_NUMERIC(0);
    FixedDecimal *arg1 = PG_GETARG_FIXDECTYPE(1);
    FixedDecimal *arg2;
    FixedDecimal *result;
    Datum val;
    val = DirectFunctionCall2(numeric_fixeddecimal, NumericGetDatum(var), -1);
    result = (FixedDecimal *)palloc(MAXFIXALIGN);
    arg2 = (FixedDecimal *)palloc(MAXFIXALIGN);
    arg2 = DatumGetFixedDecimal(val);

    val = DirectFunctionCall2(fixeddecimaldiv, FixedDecimalGetDatum(arg2), FixedDecimalGetDatum(arg1));
    result = DatumGetFixedDecimal(val);
    PG_RETURN_FIXDECTYPE(result);
}

/*Aggregate support*/

static FixedDecimalAggState *makeFixedDecimalAggState(FunctionCallInfo fcinfo) {
    FixedDecimalAggState *state;
    MemoryContext agg_context;
    MemoryContext old_context;

    if (!AggCheckCallContext(fcinfo, &agg_context))
        elog(ERROR, "aggregate function called in non-aggregate context");

    old_context = MemoryContextSwitchTo(agg_context);

    state = (FixedDecimalAggState *)palloc0(sizeof(FixedDecimalAggState));
    state->agg_context = agg_context;

    MemoryContextSwitchTo(old_context);

    return state;
}

/*
 * Accumulate a new input value for fixeddecimal aggregate functions.
 */
static void fixeddecimal_accum(FixedDecimalAggState *state, FixedDecimal *val) {
    uint8 scale_difference;
    int sign;
    int state_sign;
    int128 sumX;
    int128 newval;
    int128 result;
    sign = (ISSet(val->sign)) ? -1 : 1;
    state_sign = (ISSet(state->signofsumX)) ? -1 : 1;
    sumX = state_sign * (state->sumX);
    newval = sign * (val->value);

#ifdef HAVE_BUILTIN_OVERFLOW
    if (__builtin_add_overflow(sumX, newval, &sumX))
        ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));
    state->N++;
#else
    if (state->N++ > 0) {

        if (val->scale > state->state_scale) {
            scale_difference = get_scale_difference(state->state_scale, val->scale);
            state->state_scale = val->scale;
            sumX = sumX * get_multiplier(scale_difference);
        } else {
            scale_difference = get_scale_difference(state->state_scale, val->scale);
            newval = newval * get_multiplier(scale_difference);
        }

        result = sumX + newval;

        if (SAMESIGN(sumX, newval) && !SAMESIGN(result, sumX))
            ereport(ERROR, (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE), errmsg("fixeddecimal out of range")));

        if (result < 0) {
            state_sign = -1;
        } else {
            state_sign = 1;
        }

        state->signofsumX = state_sign;
        state->sumX = state_sign * result;
    } else {
        state->signofsumX = sign;
        state->sumX = sign * newval;
        state->state_scale = val->scale;
    }
#endif /* HAVE_BUILTIN_OVERFLOW */
}

Datum fixeddecimal_avg_accum(PG_FUNCTION_ARGS) {
    FixedDecimalAggState *state;
    FixedDecimal *result;
    state = PG_ARGISNULL(0) ? NULL : (FixedDecimalAggState *)PG_GETARG_POINTER(0);
    result = (FixedDecimal *)palloc(MAXFIXALIGN);
    /* Create the state data on the first call */
    if (state == NULL)
        state = makeFixedDecimalAggState(fcinfo);

    if (!PG_ARGISNULL(1)) {
        result = PG_GETARG_FIXDECTYPE(1);
        fixeddecimal_accum(state, result);
    }

    PG_RETURN_POINTER(state);
}

Datum fixeddecimal_avg(PG_FUNCTION_ARGS) {
    FixedDecimalAggState *state;
    FixedDecimal *result;
    int128 value;
    int sign = 1;
    state = PG_ARGISNULL(0) ? NULL : (FixedDecimalAggState *)PG_GETARG_POINTER(0);
    sign = (ISSet(state->signofsumX)) ? -1 : 1;
    /* If there were no non-null inputs, return NULL */
    if (state == NULL || state->N == 0)
        PG_RETURN_NULL();
    result = (FixedDecimal *)palloc(MAXFIXALIGN);
    result->scale = state->state_scale;
    result->sign = sign;
    value = (sign) * (state->sumX);
    value = value / state->N;
    result->value = sign * value;

    result->precision = get_precision(result->value);

    PG_RETURN_FIXDECTYPE(result);
}

Datum fixeddecimal_sum(PG_FUNCTION_ARGS) {
    FixedDecimalAggState *state;
    FixedDecimal *result;
    state = PG_ARGISNULL(0) ? NULL : (FixedDecimalAggState *)PG_GETARG_POINTER(0);
    /* If there were no non-null inputs, return NULL */
    if (state == NULL || state->N == 0)
        PG_RETURN_NULL();
    result = (FixedDecimal *)palloc(MAXFIXALIGN);
    result->scale = state->state_scale;
    result->sign = state->signofsumX;
    result->value = state->sumX;

    result->precision = get_precision(result->value);

    PG_RETURN_FIXDECTYPE(result);
}

/*
 * Input / Output / Send / Receive functions for aggrgate states
 */

Datum fixeddecimalaggstatein(PG_FUNCTION_ARGS) {
    char *str = pstrdup(PG_GETARG_CSTRING(0));
    FixedDecimalAggState *state;
    FixedDecimal *result;
    char *token;

    state = (FixedDecimalAggState *)palloc(sizeof(FixedDecimalAggState));

    token = strtok(str, ":");
    result = (FixedDecimal *)(PG_GETARG_POINTER(DirectFunctionCall3(fixeddecimalin, CStringGetDatum(token), 0, 0)));
    state->sumX = result->value;
    state->signofsumX = result->sign;
    token = strtok(NULL, ":");
    state->N = DatumGetInt64(DirectFunctionCall1(int8in, CStringGetDatum(token)));
    pfree(str);

    PG_RETURN_POINTER(state);
}

/*
 * fixeddecimalaggstateout()
 */
Datum fixeddecimalaggstateout(PG_FUNCTION_ARGS) {
    FixedDecimalAggState *state = (FixedDecimalAggState *)PG_GETARG_POINTER(0);
    FixedDecimal *result;
    char buf[MAXFIXALIGN + 1 + MAXFIXALIGN + 1];
    char *p;

    result = (FixedDecimal *)palloc0(MAXFIXALIGN);
    result->sign = state->signofsumX;
    result->value = state->sumX;
    result->scale = state->state_scale;
    result->precision = get_precision(result->value);

    p = fixeddecimal2str(result, buf);
    *p++ = ':';
    p = pg_int64tostr(p, state->N);

    PG_RETURN_CSTRING(pnstrdup(buf, p - buf));
}

/*
 *		fixeddecimalaggstaterecv
 */
Datum fixeddecimalaggstaterecv(PG_FUNCTION_ARGS) {
    StringInfo buf = (StringInfo)PG_GETARG_POINTER(0);
    FixedDecimalAggState *state;
    state = (FixedDecimalAggState *)palloc(sizeof(FixedDecimalAggState));

    state->sumX = pq_getmsgint(buf, sizeof(int64));
    state->N = pq_getmsgint(buf, sizeof(int64));

    PG_RETURN_POINTER(state);
}

/*
 *		fixeddecimalaggstatesend
 */
Datum fixeddecimalaggstatesend(PG_FUNCTION_ARGS) {
    FixedDecimalAggState *state = (FixedDecimalAggState *)PG_GETARG_POINTER(0);
    StringInfoData buf;

    pq_begintypsend(&buf);

    pq_sendint(&buf, state->sumX, sizeof(uint128));
    pq_sendint(&buf, state->N, sizeof(int64));

    PG_RETURN_BYTEA_P(pq_endtypsend(&buf));
}

Datum fixeddecimalaggstateserialize(PG_FUNCTION_ARGS) {
    FixedDecimalAggState *state;
    StringInfoData buf;
    bytea *result;

    /* Ensure we disallow calling when not in aggregate context */
    if (!AggCheckCallContext(fcinfo, NULL))
        elog(ERROR, "aggregate function called in non-aggregate context");

    state = (FixedDecimalAggState *)PG_GETARG_POINTER(0);

    pq_begintypsend(&buf);

    /* N */
    pq_sendint64(&buf, state->N);

    /* sumX */
    pq_sendint64(&buf, state->sumX);

    result = pq_endtypsend(&buf);

    PG_RETURN_BYTEA_P(result);
}

Datum fixeddecimalaggstatedeserialize(PG_FUNCTION_ARGS) {
    bytea *sstate;
    FixedDecimalAggState *result;
    StringInfoData buf;

    if (!AggCheckCallContext(fcinfo, NULL))
        elog(ERROR, "aggregate function called in non-aggregate context");

    sstate = PG_GETARG_BYTEA_P(0);

    /*
     * Copy the bytea into a StringInfo so that we can "receive" it using the
     * standard recv-function infrastructure.
     */
    initStringInfo(&buf);
    appendBinaryStringInfo(&buf, VARDATA(sstate), VARSIZE(sstate) - VARHDRSZ);

    result = (FixedDecimalAggState *)palloc(sizeof(FixedDecimalAggState));

    /* N */
    result->N = pq_getmsgint64(&buf);

    /* sumX */
    result->sumX = pq_getmsgint64(&buf);

    pq_getmsgend(&buf);
    pfree(buf.data);

    PG_RETURN_POINTER(result);
}

Datum fixeddecimalaggstatecombine(PG_FUNCTION_ARGS) {
    FixedDecimalAggState *collectstate;
    FixedDecimalAggState *transstate;
    MemoryContext agg_context;
    MemoryContext old_context;
    int128 arg1;
    int128 arg2;

    if (!AggCheckCallContext(fcinfo, &agg_context))
        elog(ERROR, "aggregate function called in non-aggregate context");

    old_context = MemoryContextSwitchTo(agg_context);

    collectstate = PG_ARGISNULL(0) ? NULL : (FixedDecimalAggState *)PG_GETARG_POINTER(0);

    if (collectstate == NULL) {
        collectstate = (FixedDecimalAggState *)palloc(sizeof(FixedDecimalAggState));
        collectstate->sumX = 0;
        collectstate->N = 0;
    }

    transstate = PG_ARGISNULL(1) ? NULL : (FixedDecimalAggState *)PG_GETARG_POINTER(1);

    if (transstate == NULL)
        PG_RETURN_POINTER(collectstate);

    if (ISSet(collectstate->signofsumX)) {
        arg1 = -1 * collectstate->sumX;
    } else {
        arg1 = collectstate->sumX;
    }

    if (ISSet(transstate->signofsumX)) {
        arg2 = -1 * transstate->sumX;
    } else {
        arg2 = transstate->sumX;
    }

    collectstate->sumX = DatumGetInt64(DirectFunctionCall2(fixeddecimalpl, Int64GetDatum(arg1), Int64GetDatum(arg2)));
    collectstate->N =
        DatumGetInt64(DirectFunctionCall2(int8pl, Int64GetDatum(collectstate->N), Int64GetDatum(transstate->N)));

    MemoryContextSwitchTo(old_context);

    PG_RETURN_POINTER(collectstate);
}