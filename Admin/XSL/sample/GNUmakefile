ADMIN_DIR=../..
include ../../GNUmakefile

EXTRA_XSL_PARAMS=-param for-instructors 1 -stringparam case-studies-icon-dir $(ADMIN_DIR)/icons -param with-exercises 1

sample.fo: sample.Rdb GNUmakefile ../fo/CaseStudies.xsl
sample.html: sample.Rdb GNUmakefile


