CREATE TABLE fixed_dec (
    c1 FIXEDDECIMAL,
    c2 FIXEDDECIMAL
);

INSERT INTO fixed_dec VALUES('NaN','inf');
INSERT INTO fixed_dec VALUES('-inf','inf');
INSERT INTO fixed_dec VALUES('Infinity','-inf');
INSERT INTO fixed_dec VALUES('-Infinity','NaN');
INSERT INTO fixed_dec VALUES('inf','-Infinity');
INSERT INTO fixed_dec VALUES('NaN','inf');
INSERT INTO fixed_dec VALUES('NaN','100');
INSERT INTO fixed_dec VALUES('Infinity','12.34');

SELECT fixeddecimalpl(c1,c2) FROM fixed_dec;
SELECT fixeddecimalmi(c1,c2) FROM fixed_dec;
SELECT fixeddecimalmul(c1,c2) FROM fixed_dec;
SELECT fixeddecimaldiv(c1,c2) FROM fixed_dec;