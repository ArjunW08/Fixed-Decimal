MODULES = fixeddecimal
OBJS = fixeddecimal.o
EXTENSION = fixeddecimal
DATA = fixeddecimal--0.0.1.sql
REGRESS = aggregate brin cast comparison index overflow fixeddecimal specialvalues
TESTS = $(wildcard regress_test/sql/*.sql)
REGRESS_OPTS = --inputdir=regress_test --outputdir=regress_test --load-extension=fixeddecimal
PG_CXXFLAGS = -fPIC -Wno-deprecated-register
PG_CONFIG = pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)