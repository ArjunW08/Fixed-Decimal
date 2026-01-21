CREATE TYPE FIXEDDECIMAL;

CREATE FUNCTION fixeddecimalin(cstring, oid, int4)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalin'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalout(fixeddecimal)
RETURNS cstring
AS 'fixeddecimal', 'fixeddecimalout'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalrecv(internal)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalrecv'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalsend(FIXEDDECIMAL)
RETURNS bytea
AS 'fixeddecimal', 'fixeddecimalsend'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimaltypmodin(_cstring)
RETURNS INT4
AS 'fixeddecimal', 'fixeddecimaltypmodin'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimaltypmodout(INT4)
RETURNS cstring
AS 'fixeddecimal', 'fixeddecimaltypmodout'
LANGUAGE C IMMUTABLE STRICT;

CREATE TYPE FIXEDDECIMAL (
    INPUT          = fixeddecimalin,
    OUTPUT         = fixeddecimalout,
    RECEIVE        = fixeddecimalrecv,
    SEND           = fixeddecimalsend,
	TYPMOD_IN      = fixeddecimaltypmodin,
	TYPMOD_OUT     = fixeddecimaltypmodout,
    INTERNALLENGTH = 32,
	ALIGNMENT      = 'double',
    STORAGE        = plain,
    CATEGORY       = 'N',
    PREFERRED      = false,
    COLLATABLE     = false
);

-- Relational Functions

CREATE FUNCTION fixeddecimaleq(FIXEDDECIMAL, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimaleq'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalne(FIXEDDECIMAL, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimalne'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimallt(FIXEDDECIMAL, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimallt'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalgt(FIXEDDECIMAL, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimalgt'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalle(FIXEDDECIMAL, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimalle'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalge(FIXEDDECIMAL, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimalge'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_cmp(FIXEDDECIMAL, FIXEDDECIMAL)
RETURNS INT4
AS 'fixeddecimal', 'fixeddecimal_cmp'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalabs(FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalabs'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimallarger(FIXEDDECIMAL, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimallarger'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalsmaller(FIXEDDECIMAL, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalsmaller'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_hash(FIXEDDECIMAL)
RETURNS INT4
AS 'fixeddecimal', 'fixeddecimal_hash'
LANGUAGE C IMMUTABLE STRICT;

CREATE OPERATOR = (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = FIXEDDECIMAL,
    COMMUTATOR = =,
    NEGATOR    = <>,
    PROCEDURE  = fixeddecimaleq,
    RESTRICT   = eqsel,
    JOIN       = eqjoinsel,
    MERGES
);

CREATE OPERATOR <> (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = =,
    COMMUTATOR = <>,
    PROCEDURE  = fixeddecimalne,
    RESTRICT   = neqsel,
    JOIN       = neqjoinsel
);

CREATE OPERATOR < (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = >=,
    COMMUTATOR = >,
    PROCEDURE  = fixeddecimallt,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR <= (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = >,
    COMMUTATOR = >=,
    PROCEDURE  = fixeddecimalle,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR >= (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = <,
    COMMUTATOR = <=,
    PROCEDURE  = fixeddecimalge,
    RESTRICT   = scalargtsel,
    JOIN       = scalargtjoinsel
);

CREATE OPERATOR > (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = <=,
    COMMUTATOR = <,
    PROCEDURE  = fixeddecimalgt,
    RESTRICT   = scalargtsel,
    JOIN       = scalargtjoinsel
);

CREATE OPERATOR CLASS fixeddecimal_ops
DEFAULT FOR TYPE FIXEDDECIMAL USING btree AS
    OPERATOR    1   <  (FIXEDDECIMAL, FIXEDDECIMAL),
    OPERATOR    2   <= (FIXEDDECIMAL, FIXEDDECIMAL),
    OPERATOR    3   =  (FIXEDDECIMAL, FIXEDDECIMAL),
    OPERATOR    4   >= (FIXEDDECIMAL, FIXEDDECIMAL),
    OPERATOR    5   >  (FIXEDDECIMAL, FIXEDDECIMAL),
    FUNCTION    1   fixeddecimal_cmp(FIXEDDECIMAL, FIXEDDECIMAL);

CREATE OPERATOR CLASS fixeddecimal_ops
DEFAULT FOR TYPE FIXEDDECIMAL USING hash AS
    OPERATOR    1   =  (FIXEDDECIMAL, FIXEDDECIMAL),
    FUNCTION    1   fixeddecimal_hash(FIXEDDECIMAL);

-- FIXEDDECIMAL, INT2

CREATE FUNCTION fixeddecimal_int2_cmp(FIXEDDECIMAL, INT2)
RETURNS INT4
AS 'fixeddecimal', 'fixeddecimal_int2_cmp'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_int2_eq(FIXEDDECIMAL, INT2)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_int2_eq'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_int2_ne(FIXEDDECIMAL, INT2)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_int2_ne'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_int2_lt(FIXEDDECIMAL, INT2)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_int2_lt'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_int2_le(FIXEDDECIMAL, INT2)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_int2_le'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_int2_gt(FIXEDDECIMAL, INT2)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_int2_gt'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_int2_ge(FIXEDDECIMAL, INT2)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_int2_ge'
LANGUAGE C IMMUTABLE STRICT;

CREATE OPERATOR = (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT2,
    COMMUTATOR = =,
    NEGATOR    = <>,
    PROCEDURE  = fixeddecimal_int2_eq,
    RESTRICT   = eqsel,
    JOIN       = eqjoinsel,
    MERGES
);

CREATE OPERATOR <> (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT2,
    NEGATOR    = =,
    COMMUTATOR = <>,
    PROCEDURE  = fixeddecimal_int2_ne,
    RESTRICT   = neqsel,
    JOIN       = neqjoinsel
);

CREATE OPERATOR < (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT2,
    NEGATOR    = >=,
    COMMUTATOR = >,
    PROCEDURE  = fixeddecimal_int2_lt,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR <= (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT2,
    NEGATOR    = >,
    COMMUTATOR = >=,
    PROCEDURE  = fixeddecimal_int2_le,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR >= (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT2,
    NEGATOR    = <,
    COMMUTATOR = <=,
    PROCEDURE  = fixeddecimal_int2_ge,
    RESTRICT   = scalargtsel,
   JOIN       = scalargtjoinsel
);

CREATE OPERATOR > (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT2,
    NEGATOR    = <=,
    COMMUTATOR = <,
    PROCEDURE  = fixeddecimal_int2_gt,
    RESTRICT   = scalargtsel,
    JOIN       = scalargtjoinsel
);

CREATE OPERATOR CLASS fixeddecimal_int2_ops
FOR TYPE FIXEDDECIMAL USING btree AS
    OPERATOR    1   <  (FIXEDDECIMAL, INT2),
    OPERATOR    2   <= (FIXEDDECIMAL, INT2),
    OPERATOR    3   =  (FIXEDDECIMAL, INT2),
    OPERATOR    4   >= (FIXEDDECIMAL, INT2),
    OPERATOR    5   >  (FIXEDDECIMAL, INT2),
    FUNCTION    1   fixeddecimal_int2_cmp(FIXEDDECIMAL, INT2);

CREATE OPERATOR CLASS fixeddecimal_int2_ops
FOR TYPE FIXEDDECIMAL USING hash AS
    OPERATOR    1   =  (FIXEDDECIMAL, INT2),
    FUNCTION    1   fixeddecimal_hash(FIXEDDECIMAL);

-- INT2, FIXEDDECIMAL
CREATE FUNCTION int2_fixeddecimal_cmp(INT2, FIXEDDECIMAL)
RETURNS INT4
AS 'fixeddecimal', 'int2_fixeddecimal_cmp'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int2_fixeddecimal_eq(INT2, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'int2_fixeddecimal_eq'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int2_fixeddecimal_ne(INT2, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'int2_fixeddecimal_ne'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int2_fixeddecimal_lt(INT2, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'int2_fixeddecimal_lt'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int2_fixeddecimal_le(INT2, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'int2_fixeddecimal_le'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int2_fixeddecimal_gt(INT2, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'int2_fixeddecimal_gt'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int2_fixeddecimal_ge(INT2, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'int2_fixeddecimal_ge'
LANGUAGE C IMMUTABLE STRICT;

CREATE OPERATOR = (
    LEFTARG    = INT2,
    RIGHTARG   = FIXEDDECIMAL,
    COMMUTATOR = =,
    NEGATOR    = <>,
    PROCEDURE  = int2_fixeddecimal_eq,
    RESTRICT   = eqsel,
    JOIN       = eqjoinsel,
    MERGES
);

CREATE OPERATOR <> (
    LEFTARG    = INT2,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = =,
    COMMUTATOR = <>,
    PROCEDURE  = int2_fixeddecimal_ne,
    RESTRICT   = neqsel,
    JOIN       = neqjoinsel
);

CREATE OPERATOR < (
    LEFTARG    = INT2,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = >=,
    COMMUTATOR = >,
    PROCEDURE  = int2_fixeddecimal_lt,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR <= (
    LEFTARG    = INT2,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = >,
    COMMUTATOR = >=,
    PROCEDURE  = int2_fixeddecimal_le,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR >= (
    LEFTARG    = INT2,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = <,
    COMMUTATOR = <=,
    PROCEDURE  = int2_fixeddecimal_ge,
    RESTRICT   = scalargtsel,
    JOIN       = scalargtjoinsel
);

CREATE OPERATOR > (
    LEFTARG    = INT2,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = <=,
    COMMUTATOR = <,
    PROCEDURE  = int2_fixeddecimal_gt,
    RESTRICT   = scalargtsel,
    JOIN       = scalargtjoinsel
);

CREATE OPERATOR CLASS int2_fixeddecimal_ops
FOR TYPE FIXEDDECIMAL USING btree AS
    OPERATOR    1   <  (INT2, FIXEDDECIMAL),
    OPERATOR    2   <= (INT2, FIXEDDECIMAL),
    OPERATOR    3   =  (INT2, FIXEDDECIMAL),
    OPERATOR    4   >= (INT2, FIXEDDECIMAL),
    OPERATOR    5   >  (INT2, FIXEDDECIMAL),
    FUNCTION    1   int2_fixeddecimal_cmp(INT2, FIXEDDECIMAL);

CREATE OPERATOR CLASS int2_fixeddecimal_ops
FOR TYPE FIXEDDECIMAL USING hash AS
    OPERATOR    1   =  (INT2, FIXEDDECIMAL),
    FUNCTION    1   fixeddecimal_hash(FIXEDDECIMAL);

-- FIXEDDECIMAL, INT4

CREATE FUNCTION fixeddecimal_int4_cmp(FIXEDDECIMAL, INT4)
RETURNS INT4
AS 'fixeddecimal', 'fixeddecimal_int4_cmp'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_int4_eq(FIXEDDECIMAL, INT4)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_int4_eq'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_int4_ne(FIXEDDECIMAL, INT4)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_int4_ne'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_int4_lt(FIXEDDECIMAL, INT4)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_int4_lt'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_int4_le(FIXEDDECIMAL, INT4)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_int4_le'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_int4_gt(FIXEDDECIMAL, INT4)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_int4_gt'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_int4_ge(FIXEDDECIMAL, INT4)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_int4_ge'
LANGUAGE C IMMUTABLE STRICT;

CREATE OPERATOR = (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT4,
    COMMUTATOR = =,
    NEGATOR    = <>,
    PROCEDURE  = fixeddecimal_int4_eq,
    RESTRICT   = eqsel,
    JOIN       = eqjoinsel,
    MERGES
);

CREATE OPERATOR <> (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT4,
    NEGATOR    = =,
    COMMUTATOR = <>,
    PROCEDURE  = fixeddecimal_int4_ne,
    RESTRICT   = neqsel,
    JOIN       = neqjoinsel
);

CREATE OPERATOR < (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT4,
    NEGATOR    = >=,
    COMMUTATOR = >,
    PROCEDURE  = fixeddecimal_int4_lt,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR <= (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT4,
    NEGATOR    = >,
    COMMUTATOR = >=,
    PROCEDURE  = fixeddecimal_int4_le,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR >= (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT4,
    NEGATOR    = <,
    COMMUTATOR = <=,
    PROCEDURE  = fixeddecimal_int4_ge,
    RESTRICT   = scalargtsel,
    JOIN       = scalargtjoinsel
);

CREATE OPERATOR > (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT4,
    NEGATOR    = <=,
    COMMUTATOR = <,
    PROCEDURE  = fixeddecimal_int4_gt,
    RESTRICT   = scalargtsel,
    JOIN       = scalargtjoinsel
);

CREATE OPERATOR CLASS fixeddecimal_int4_ops
FOR TYPE FIXEDDECIMAL USING btree AS
    OPERATOR    1   <  (FIXEDDECIMAL, INT4),
    OPERATOR    2   <= (FIXEDDECIMAL, INT4),
    OPERATOR    3   =  (FIXEDDECIMAL, INT4),
    OPERATOR    4   >= (FIXEDDECIMAL, INT4),
    OPERATOR    5   >  (FIXEDDECIMAL, INT4),
    FUNCTION    1   fixeddecimal_int4_cmp(FIXEDDECIMAL, INT4);

CREATE OPERATOR CLASS fixeddecimal_int4_ops
FOR TYPE FIXEDDECIMAL USING hash AS
    OPERATOR    1   =  (FIXEDDECIMAL, INT4),
    FUNCTION    1   fixeddecimal_hash(FIXEDDECIMAL);

-- INT4, FIXEDDECIMAL

CREATE FUNCTION int4_fixeddecimal_cmp(INT4, FIXEDDECIMAL)
RETURNS INT4
AS 'fixeddecimal', 'int4_fixeddecimal_cmp'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int4_fixeddecimal_eq(INT4, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'int4_fixeddecimal_eq'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int4_fixeddecimal_ne(INT4, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'int4_fixeddecimal_ne'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int4_fixeddecimal_lt(INT4, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'int4_fixeddecimal_lt'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int4_fixeddecimal_le(INT4, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'int4_fixeddecimal_le'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int4_fixeddecimal_gt(INT4, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'int4_fixeddecimal_gt'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int4_fixeddecimal_ge(INT4, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'int4_fixeddecimal_ge'
LANGUAGE C IMMUTABLE STRICT;
CREATE OPERATOR = (
    LEFTARG    = INT4,
    RIGHTARG   = FIXEDDECIMAL,
    COMMUTATOR = =,
    NEGATOR    = <>,
    PROCEDURE  = int4_fixeddecimal_eq,
    RESTRICT   = eqsel,
    JOIN       = eqjoinsel,
    MERGES
);

CREATE OPERATOR <> (
    LEFTARG    = INT4,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = =,
    COMMUTATOR = <>,
    PROCEDURE  = int4_fixeddecimal_ne,
    RESTRICT   = neqsel,
    JOIN       = neqjoinsel
);

CREATE OPERATOR < (
    LEFTARG    = INT4,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = >=,
    COMMUTATOR = >,
    PROCEDURE  = int4_fixeddecimal_lt,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR <= (
    LEFTARG    = INT4,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = >,
    COMMUTATOR = >=,
    PROCEDURE  = int4_fixeddecimal_le,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR >= (
    LEFTARG    = INT4,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = <,
    COMMUTATOR = <=,
    PROCEDURE  = int4_fixeddecimal_ge,
    RESTRICT   = scalargtsel,
    JOIN       = scalargtjoinsel
);

CREATE OPERATOR > (
    LEFTARG    = INT4,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = <=,
    COMMUTATOR = <,
    PROCEDURE  = int4_fixeddecimal_gt,
    RESTRICT   = scalargtsel,
    JOIN       = scalargtjoinsel
);

CREATE OPERATOR CLASS int4_fixeddecimal_ops
FOR TYPE FIXEDDECIMAL USING btree AS
    OPERATOR    1   <  (INT4, FIXEDDECIMAL),
    OPERATOR    2   <= (INT4, FIXEDDECIMAL),
    OPERATOR    3   =  (INT4, FIXEDDECIMAL),
    OPERATOR    4   >= (INT4, FIXEDDECIMAL),
    OPERATOR    5   >  (INT4, FIXEDDECIMAL),
    FUNCTION    1   int4_fixeddecimal_cmp(INT4, FIXEDDECIMAL);

CREATE OPERATOR CLASS int4_fixeddecimal_ops
FOR TYPE FIXEDDECIMAL USING hash AS
    OPERATOR    1   =  (INT4, FIXEDDECIMAL),
    FUNCTION    1   fixeddecimal_hash(FIXEDDECIMAL);

CREATE FUNCTION fixeddecimal_numeric_cmp(FIXEDDECIMAL, NUMERIC)
RETURNS INT4
AS 'fixeddecimal', 'fixeddecimal_numeric_cmp'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION numeric_fixeddecimal_cmp(NUMERIC, FIXEDDECIMAL)
RETURNS INT4
AS 'fixeddecimal', 'numeric_fixeddecimal_cmp'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_numeric_eq(FIXEDDECIMAL, NUMERIC)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_numeric_eq'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_numeric_ne(FIXEDDECIMAL, NUMERIC)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_numeric_ne'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_numeric_lt(FIXEDDECIMAL, NUMERIC)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_numeric_lt'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_numeric_le(FIXEDDECIMAL, NUMERIC)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_numeric_le'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_numeric_gt(FIXEDDECIMAL, NUMERIC)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_numeric_gt'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_numeric_ge(FIXEDDECIMAL, NUMERIC)
RETURNS bool
AS 'fixeddecimal', 'fixeddecimal_numeric_ge'
LANGUAGE C IMMUTABLE STRICT;

CREATE OPERATOR = (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = NUMERIC,
    COMMUTATOR = =,
    NEGATOR    = <>,
    PROCEDURE  = fixeddecimal_numeric_eq,
    RESTRICT   = eqsel,
    JOIN       = eqjoinsel,
    MERGES
);

CREATE OPERATOR <> (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = NUMERIC,
    NEGATOR    = =,
    COMMUTATOR = <>,
    PROCEDURE  = fixeddecimal_numeric_ne,
    RESTRICT   = neqsel,
    JOIN       = neqjoinsel
);

CREATE OPERATOR < (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = NUMERIC,
    NEGATOR    = >=,
    COMMUTATOR = >,
    PROCEDURE  = fixeddecimal_numeric_lt,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR <= (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = NUMERIC,
    NEGATOR    = >,
    COMMUTATOR = >=,
    PROCEDURE  = fixeddecimal_numeric_le,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR >= (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = NUMERIC,
    NEGATOR    = <,
    COMMUTATOR = <=,
    PROCEDURE  = fixeddecimal_numeric_ge,
    RESTRICT   = scalargtsel,
    JOIN       = scalargtjoinsel
);

CREATE OPERATOR > (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = NUMERIC,
    NEGATOR    = <=,
    COMMUTATOR = <,
    PROCEDURE  = fixeddecimal_numeric_gt,
    RESTRICT   = scalargtsel,
    JOIN       = scalargtjoinsel
);

CREATE OPERATOR CLASS fixeddecimal_numeric_ops
FOR TYPE FIXEDDECIMAL USING btree AS
    OPERATOR    1   <  (FIXEDDECIMAL, NUMERIC),
    OPERATOR    2   <= (FIXEDDECIMAL, NUMERIC),
    OPERATOR    3   =  (FIXEDDECIMAL, NUMERIC),
    OPERATOR    4   >= (FIXEDDECIMAL, NUMERIC),
    OPERATOR    5   >  (FIXEDDECIMAL, NUMERIC),
    FUNCTION    1   fixeddecimal_numeric_cmp(FIXEDDECIMAL, NUMERIC);

CREATE OPERATOR CLASS fixeddecimal_numeric_ops
FOR TYPE FIXEDDECIMAL USING hash AS
    OPERATOR    1   =  (FIXEDDECIMAL, NUMERIC),
    FUNCTION    1   fixeddecimal_hash(FIXEDDECIMAL);

-- NUMERIC, FIXEDDECIMAL
CREATE FUNCTION numeric_fixeddecimal_eq(NUMERIC, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'numeric_fixeddecimal_eq'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION numeric_fixeddecimal_ne(NUMERIC, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'numeric_fixeddecimal_ne'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION numeric_fixeddecimal_lt(NUMERIC, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'numeric_fixeddecimal_lt'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION numeric_fixeddecimal_le(NUMERIC, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'numeric_fixeddecimal_le'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION numeric_fixeddecimal_gt(NUMERIC, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'numeric_fixeddecimal_gt'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION numeric_fixeddecimal_ge(NUMERIC, FIXEDDECIMAL)
RETURNS bool
AS 'fixeddecimal', 'numeric_fixeddecimal_ge'
LANGUAGE C IMMUTABLE STRICT;

CREATE OPERATOR = (
    LEFTARG    = NUMERIC,
    RIGHTARG   = FIXEDDECIMAL,
    COMMUTATOR = =,
    NEGATOR    = <>,
    PROCEDURE  = numeric_fixeddecimal_eq,
    RESTRICT   = eqsel,
    JOIN       = eqjoinsel,
    MERGES
);

CREATE OPERATOR <> (
    LEFTARG    = NUMERIC,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = =,
    COMMUTATOR = <>,
    PROCEDURE  = numeric_fixeddecimal_ne,
    RESTRICT   = neqsel,
    JOIN       = neqjoinsel
);

CREATE OPERATOR < (
    LEFTARG    = NUMERIC,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = >=,
    COMMUTATOR = >,
    PROCEDURE  = numeric_fixeddecimal_lt,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR <= (
    LEFTARG    = NUMERIC,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = >,
    COMMUTATOR = >=,
    PROCEDURE  = numeric_fixeddecimal_le,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR >= (
    LEFTARG    = NUMERIC,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = <,
    COMMUTATOR = <=,
    PROCEDURE  = numeric_fixeddecimal_ge,
    RESTRICT   = scalargtsel,
    JOIN       = scalargtjoinsel
);

CREATE OPERATOR > (
    LEFTARG    = NUMERIC,
    RIGHTARG   = FIXEDDECIMAL,
    NEGATOR    = <=,
    COMMUTATOR = <,
    PROCEDURE  = numeric_fixeddecimal_gt,
    RESTRICT   = scalargtsel,
    JOIN       = scalargtjoinsel
);

CREATE OPERATOR CLASS numeric_fixeddecimal_ops
FOR TYPE FIXEDDECIMAL USING btree AS
    OPERATOR    1   <  (NUMERIC, FIXEDDECIMAL) FOR SEARCH,
    OPERATOR    2   <= (NUMERIC, FIXEDDECIMAL) FOR SEARCH,
    OPERATOR    3   =  (NUMERIC, FIXEDDECIMAL) FOR SEARCH,
    OPERATOR    4   >= (NUMERIC, FIXEDDECIMAL) FOR SEARCH,
    OPERATOR    5   >  (NUMERIC, FIXEDDECIMAL) FOR SEARCH,
    FUNCTION    1   numeric_fixeddecimal_cmp(NUMERIC, FIXEDDECIMAL);

CREATE OPERATOR CLASS numeric_fixeddecimal_ops
FOR TYPE FIXEDDECIMAL USING hash AS
    OPERATOR    1   =  (NUMERIC, FIXEDDECIMAL),
    FUNCTION    1   fixeddecimal_hash(FIXEDDECIMAL);

-- Base functions

CREATE FUNCTION fixeddecimalum(FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalum'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalpl(FIXEDDECIMAL, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalpl'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalmi(FIXEDDECIMAL, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalmi'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalmul(FIXEDDECIMAL, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalmul'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimaldiv(FIXEDDECIMAL, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimaldiv'
LANGUAGE C IMMUTABLE STRICT;

-- Base Operators

CREATE OPERATOR + (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = FIXEDDECIMAL,
    COMMUTATOR = +,
    PROCEDURE  = fixeddecimalpl
);

CREATE OPERATOR - (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = FIXEDDECIMAL,
    PROCEDURE  = fixeddecimalmi
);

CREATE OPERATOR * (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = FIXEDDECIMAL,
    COMMUTATOR = *,
    PROCEDURE  = fixeddecimalmul
);

CREATE OPERATOR / (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = FIXEDDECIMAL,
    PROCEDURE  = fixeddecimaldiv
);

--
-- Cross type operators with int4
--

CREATE FUNCTION fixeddecimalint4pl(FIXEDDECIMAL, INT4)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalint4pl'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalint4mi(FIXEDDECIMAL, INT4)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalint4mi'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalint4mul(FIXEDDECIMAL, INT4)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalint4mul'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalint4div(FIXEDDECIMAL, INT4)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalint4div'
LANGUAGE C IMMUTABLE STRICT;

CREATE OPERATOR + (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT4,
    COMMUTATOR = +,
    PROCEDURE  = fixeddecimalint4pl
);

CREATE OPERATOR - (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT4,
    PROCEDURE  = fixeddecimalint4mi
);

CREATE OPERATOR * (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT4,
    COMMUTATOR = *,
    PROCEDURE  = fixeddecimalint4mul
);

CREATE OPERATOR / (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT4,
    PROCEDURE  = fixeddecimalint4div
);


CREATE FUNCTION int4fixeddecimalpl(INT4, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'int4fixeddecimalpl'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int4fixeddecimalmi(INT4, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'int4fixeddecimalmi'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int4fixeddecimalmul(INT4, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'int4fixeddecimalmul'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int4fixeddecimaldiv(INT4, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'int4fixeddecimaldiv'
LANGUAGE C IMMUTABLE STRICT;

CREATE OPERATOR + (
    LEFTARG    = INT4,
    RIGHTARG   = FIXEDDECIMAL,
    COMMUTATOR = +,
    PROCEDURE  = int4fixeddecimalpl
);

CREATE OPERATOR - (
    LEFTARG    = INT4,
    RIGHTARG   = FIXEDDECIMAL,
    PROCEDURE  = int4fixeddecimalmi
);

CREATE OPERATOR * (
    LEFTARG    = INT4,
    RIGHTARG   = FIXEDDECIMAL,
    COMMUTATOR = *,
    PROCEDURE  = int4fixeddecimalmul
);

CREATE OPERATOR / (
    LEFTARG    = INT4,
    RIGHTARG   = FIXEDDECIMAL,
    PROCEDURE  = int4fixeddecimaldiv
);

--
-- Cross type operators with int2
--

CREATE FUNCTION fixeddecimalint2pl(FIXEDDECIMAL, INT2)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalint2pl'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalint2mi(FIXEDDECIMAL, INT2)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalint2mi'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalint2mul(FIXEDDECIMAL, INT2)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalint2mul'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalint2div(FIXEDDECIMAL, INT2)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalint2div'
LANGUAGE C IMMUTABLE STRICT;

CREATE OPERATOR + (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT2,
    COMMUTATOR = +,
    PROCEDURE  = fixeddecimalint2pl
);

CREATE OPERATOR - (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT2,
    PROCEDURE  = fixeddecimalint2mi
);

CREATE OPERATOR * (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT2,
    COMMUTATOR = *,
    PROCEDURE  = fixeddecimalint2mul
);

CREATE OPERATOR / (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = INT2,
    PROCEDURE  = fixeddecimalint2div
);

CREATE FUNCTION int2fixeddecimalpl(INT2, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'int2fixeddecimalpl'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int2fixeddecimalmi(INT2, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'int2fixeddecimalmi'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int2fixeddecimalmul(INT2, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'int2fixeddecimalmul'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int2fixeddecimaldiv(INT2, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'int2fixeddecimaldiv'
LANGUAGE C IMMUTABLE STRICT;

CREATE OPERATOR + (
    LEFTARG    = INT2,
    RIGHTARG   = FIXEDDECIMAL,
    COMMUTATOR = +,
    PROCEDURE  = int2fixeddecimalpl
);

CREATE OPERATOR - (
    LEFTARG    = INT2,
    RIGHTARG   = FIXEDDECIMAL,
    PROCEDURE  = int2fixeddecimalmi
);

CREATE OPERATOR * (
    LEFTARG    = INT2,
    RIGHTARG   = FIXEDDECIMAL,
    COMMUTATOR = *,
    PROCEDURE  = int2fixeddecimalmul
);

CREATE OPERATOR / (
    LEFTARG    = INT2,
    RIGHTARG   = FIXEDDECIMAL,
    PROCEDURE  = int2fixeddecimaldiv
);

CREATE FUNCTION numericfixeddecimalpl(NUMERIC, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'numericfixeddecimalpl'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION numericfixeddecimalmi(NUMERIC, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'numericfixeddecimalmi'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION numericfixeddecimalmul(NUMERIC, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'numericfixeddecimalmul'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION numericfixeddecimaldiv(NUMERIC, FIXEDDECIMAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'numericfixeddecimaldiv'
LANGUAGE C IMMUTABLE STRICT;

CREATE OPERATOR + (
    LEFTARG    = NUMERIC,
    RIGHTARG   = FIXEDDECIMAL,
    COMMUTATOR = +,
    PROCEDURE  = numericfixeddecimalpl
);

CREATE OPERATOR - (
    LEFTARG    = NUMERIC,
    RIGHTARG   = FIXEDDECIMAL,
    PROCEDURE  = numericfixeddecimalmi
);

CREATE OPERATOR * (
    LEFTARG    = NUMERIC,
    RIGHTARG   = FIXEDDECIMAL,
    COMMUTATOR = *,
    PROCEDURE  = numericfixeddecimalmul
);

CREATE OPERATOR / (
    LEFTARG    = NUMERIC,
    RIGHTARG   = FIXEDDECIMAL,
    PROCEDURE  = numericfixeddecimaldiv
);

CREATE FUNCTION fixeddecimalnumericpl(FIXEDDECIMAL, NUMERIC)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalnumericpl'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalnumericmi(FIXEDDECIMAL, NUMERIC)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalnumericmi'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalnumericmul(FIXEDDECIMAL, NUMERIC)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalnumericmul'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalnumericdiv(FIXEDDECIMAL, NUMERIC)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimalnumericdiv'
LANGUAGE C IMMUTABLE STRICT;

CREATE OPERATOR + (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = NUMERIC,
    COMMUTATOR = +,
    PROCEDURE  = fixeddecimalnumericpl
);

CREATE OPERATOR - (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = NUMERIC,
    PROCEDURE  = fixeddecimalnumericmi
);

CREATE OPERATOR * (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = NUMERIC,
    COMMUTATOR = *,
    PROCEDURE  = fixeddecimalnumericmul
);

CREATE OPERATOR / (
    LEFTARG    = FIXEDDECIMAL,
    RIGHTARG   = NUMERIC,
    PROCEDURE  = fixeddecimalnumericdiv
);

CREATE OPERATOR CLASS fixeddecimal_minmax_ops
DEFAULT FOR TYPE FIXEDDECIMAL USING brin AS
    OPERATOR    1   <  (FIXEDDECIMAL, FIXEDDECIMAL),
    OPERATOR    2   <= (FIXEDDECIMAL, FIXEDDECIMAL),
    OPERATOR    3   =  (FIXEDDECIMAL, FIXEDDECIMAL),
    OPERATOR    4   >= (FIXEDDECIMAL, FIXEDDECIMAL),
    OPERATOR    5   >  (FIXEDDECIMAL, FIXEDDECIMAL),
    FUNCTION    1   brin_minmax_opcinfo(INTERNAL),
    FUNCTION    2   brin_minmax_add_value(INTERNAL, INTERNAL, INTERNAL, INTERNAL),
    FUNCTION    3   brin_minmax_consistent(INTERNAL, INTERNAL, INTERNAL),
    FUNCTION    4   brin_minmax_union(INTERNAL, INTERNAL, INTERNAL);

-- Aggregate Functions

CREATE TYPE FIXEDDECIMALAGGSTATE;

CREATE FUNCTION fixeddecimalaggstatein(cstring, oid, int4)
RETURNS FIXEDDECIMALAGGSTATE
AS 'fixeddecimal', 'fixeddecimalaggstatein'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalaggstateout(fixeddecimalaggstate)
RETURNS cstring
AS 'fixeddecimal', 'fixeddecimalaggstateout'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalaggstaterecv(internal)
RETURNS FIXEDDECIMALAGGSTATE
AS 'fixeddecimal', 'fixeddecimalaggstaterecv'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalaggstatesend(FIXEDDECIMALAGGSTATE)
RETURNS bytea
AS 'fixeddecimal', 'fixeddecimalaggstatesend'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalaggstatecombine(FIXEDDECIMALAGGSTATE, FIXEDDECIMALAGGSTATE)
RETURNS FIXEDDECIMALAGGSTATE
AS 'fixeddecimal', 'fixeddecimalaggstatecombine'
LANGUAGE C IMMUTABLE;

CREATE FUNCTION fixeddecimalaggstateserialize(INTERNAL)
RETURNS BYTEA
AS 'fixeddecimal', 'fixeddecimalaggstateserialize'
LANGUAGE C IMMUTABLE PARALLEL SAFE;

CREATE FUNCTION fixeddecimalaggstatedeserialize(BYTEA, INTERNAL)
RETURNS INTERNAL
AS 'fixeddecimal', 'fixeddecimalaggstatedeserialize'
LANGUAGE C IMMUTABLE PARALLEL SAFE;

CREATE FUNCTION fixeddecimal_avg_accum(FIXEDDECIMALAGGSTATE, FIXEDDECIMAL)
RETURNS FIXEDDECIMALAGGSTATE
AS 'fixeddecimal', 'fixeddecimal_avg_accum'
LANGUAGE C IMMUTABLE;

CREATE FUNCTION fixeddecimal_sum(FIXEDDECIMALAGGSTATE)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimal_sum'
LANGUAGE C IMMUTABLE;

CREATE FUNCTION fixeddecimal_avg(FIXEDDECIMALAGGSTATE)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimal_avg'
LANGUAGE C IMMUTABLE;

CREATE FUNCTION fixeddecimal_avg_accum(INTERNAL, FIXEDDECIMAL)
RETURNS INTERNAL
AS 'fixeddecimal', 'fixeddecimal_avg_accum'
LANGUAGE C IMMUTABLE;

CREATE FUNCTION fixeddecimal_sum(INTERNAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimal_sum'
LANGUAGE C IMMUTABLE;

CREATE FUNCTION fixeddecimal_avg(INTERNAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimal_avg'
LANGUAGE C IMMUTABLE;

CREATE AGGREGATE min(FIXEDDECIMAL) (
    SFUNC = fixeddecimalsmaller,
    STYPE = FIXEDDECIMAL,
    SORTOP = <
);

CREATE AGGREGATE max(FIXEDDECIMAL) (
    SFUNC = fixeddecimallarger,
    STYPE = FIXEDDECIMAL,
    SORTOP = >
);

CREATE AGGREGATE sum(FIXEDDECIMAL) (
    SFUNC = fixeddecimal_avg_accum,
	FINALFUNC = fixeddecimal_sum,
    STYPE = INTERNAL
);

CREATE AGGREGATE avg(FIXEDDECIMAL) (
    SFUNC = fixeddecimal_avg_accum,
	FINALFUNC = fixeddecimal_avg,
    STYPE = INTERNAL
);

UPDATE pg_proc SET proparallel = 's'
WHERE oid = 'min(FIXEDDECIMAL)'::pg_catalog.regprocedure;

UPDATE pg_proc SET proparallel = 's'
WHERE oid = 'max(FIXEDDECIMAL)'::pg_catalog.regprocedure;

UPDATE pg_proc SET proparallel = 's'
WHERE oid = 'sum(FIXEDDECIMAL)'::pg_catalog.regprocedure;

UPDATE pg_proc SET proparallel = 's'
WHERE oid = 'avg(FIXEDDECIMAL)'::pg_catalog.regprocedure;

UPDATE pg_aggregate SET aggcombinefn = 'fixeddecimalsmaller'
WHERE aggfnoid = 'min(FIXEDDECIMAL)'::pg_catalog.regprocedure;

UPDATE pg_aggregate SET aggcombinefn = 'fixeddecimallarger'
WHERE aggfnoid = 'max(FIXEDDECIMAL)'::pg_catalog.regprocedure;

UPDATE pg_aggregate SET aggcombinefn = 'fixeddecimalaggstatecombine',
						aggserialfn = 'fixeddecimalaggstateserialize',
						aggdeserialfn = 'fixeddecimalaggstatedeserialize'
WHERE aggfnoid = 'sum(FIXEDDECIMAL)'::pg_catalog.regprocedure;

UPDATE pg_aggregate SET aggcombinefn = 'fixeddecimalaggstatecombine',
						aggserialfn = 'fixeddecimalaggstateserialize',
						aggdeserialfn = 'fixeddecimalaggstatedeserialize'
WHERE aggfnoid = 'avg(FIXEDDECIMAL)'::pg_catalog.regprocedure;


CREATE TYPE FIXEDDECIMALAGGSTATE (
    INPUT          = fixeddecimalaggstatein,
    OUTPUT         = fixeddecimalaggstateout,
    RECEIVE        = fixeddecimalaggstaterecv,
    SEND           = fixeddecimalaggstatesend,
    INTERNALLENGTH = 48,
    ALIGNMENT      = 'double',
    STORAGE        = plain,
    CATEGORY       = 'N',
    PREFERRED      = false,
    COLLATABLE     = false
);

-- Conversion Functions

CREATE FUNCTION int4fixeddecimal(INT4)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'int4fixeddecimal'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION int2fixeddecimal(INT2)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'int2fixeddecimal'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION dtofixeddecimal(DOUBLE PRECISION)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'dtofixeddecimal'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION ftofixeddecimal(REAL)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'ftofixeddecimal'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION numeric_fixeddecimal(NUMERIC, INT4)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'numeric_fixeddecimal'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalint4(FIXEDDECIMAL)
RETURNS INT4
AS 'fixeddecimal', 'fixeddecimalint4'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimalint2(FIXEDDECIMAL)
RETURNS INT2
AS 'fixeddecimal', 'fixeddecimalint2'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimaltod(FIXEDDECIMAL)
RETURNS DOUBLE PRECISION
AS 'fixeddecimal', 'fixeddecimaltod'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimaltof(FIXEDDECIMAL)
RETURNS REAL
AS 'fixeddecimal', 'fixeddecimaltof'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal_numeric(FIXEDDECIMAL)
RETURNS NUMERIC
AS 'fixeddecimal', 'fixeddecimal_numeric'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION fixeddecimal(FIXEDDECIMAL,INT4)
RETURNS FIXEDDECIMAL
AS 'fixeddecimal', 'fixeddecimal'
LANGUAGE C IMMUTABLE STRICT;

-- Casting Functions

CREATE CAST (FIXEDDECIMAL AS FIXEDDECIMAL)
	WITH FUNCTION fixeddecimal (FIXEDDECIMAL, INT4) AS ASSIGNMENT;

CREATE CAST (INT4 AS FIXEDDECIMAL)
	WITH FUNCTION int4fixeddecimal (INT4) AS IMPLICIT;

CREATE CAST (INT2 AS FIXEDDECIMAL)
	WITH FUNCTION int2fixeddecimal (INT2) AS IMPLICIT;

CREATE CAST (DOUBLE PRECISION AS FIXEDDECIMAL)
	WITH FUNCTION dtofixeddecimal (DOUBLE PRECISION) AS ASSIGNMENT;

CREATE CAST (REAL AS FIXEDDECIMAL)
	WITH FUNCTION ftofixeddecimal (REAL) AS ASSIGNMENT;

CREATE CAST (NUMERIC AS FIXEDDECIMAL)
	WITH FUNCTION numeric_fixeddecimal (NUMERIC, INT4) AS ASSIGNMENT;

CREATE CAST (FIXEDDECIMAL AS INT4)
	WITH FUNCTION fixeddecimalint4 (FIXEDDECIMAL) AS ASSIGNMENT;

CREATE CAST (FIXEDDECIMAL AS INT2)
	WITH FUNCTION fixeddecimalint2 (FIXEDDECIMAL) AS ASSIGNMENT;

CREATE CAST (FIXEDDECIMAL AS DOUBLE PRECISION)
	WITH FUNCTION fixeddecimaltod (FIXEDDECIMAL) AS IMPLICIT;

CREATE CAST (FIXEDDECIMAL AS REAL)
	WITH FUNCTION fixeddecimaltof (FIXEDDECIMAL) AS IMPLICIT;

CREATE CAST (FIXEDDECIMAL AS NUMERIC)
	WITH FUNCTION fixeddecimal_numeric (FIXEDDECIMAL) AS IMPLICIT;