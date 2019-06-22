
#include <string.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "dynlib.h"

static uim_lisp
uim_test0003_wildcard_match(uim_lisp seq_, uim_lisp pat_)
{
	while (1) {
		if (NULLP(seq_)) {
			while (CONSP(pat_) && (strcmp(REFER_C_STR(CAR(pat_)), "*") == 0)) {
				pat_ = CDR(pat_);
			}
			return NULLP(pat_) ? uim_scm_t() : uim_scm_f();
		}
		if (NULLP(pat_)) {
			return uim_scm_f();
		}
		if (strcmp(REFER_C_STR(uim_scm_car(pat_)), "*") == 0) {
			return (TRUEP(uim_test0003_wildcard_match(seq_, CDR(pat_))) ||
			        TRUEP(uim_test0003_wildcard_match(CDR(seq_), pat_))) ?
				uim_scm_t() : uim_scm_f();;
		}
		if ((strcmp(REFER_C_STR(CAR(seq_)), REFER_C_STR(CAR(pat_))) == 0) ||
		    (strcmp(REFER_C_STR(CAR(pat_)), "?") == 0)) {
			seq_ = CDR(seq_);
			pat_ = CDR(pat_);
			continue;
		}
		return uim_scm_f();
	}
}

static uim_lisp
uim_test0003_make_cands_wildcard(uim_lisp seq_, uim_lisp rule_, uim_lisp prediction_)
{
	if (TRUEP(prediction_)) {
		/* append "*" */
		seq_ = uim_scm_callf("reverse", "o",
		                     CONS(MAKE_STR("*"), uim_scm_callf("reverse", "o", seq_)));
	}
	uim_lisp cands_ = uim_scm_null();
	while (CONSP(rule_)) {
		uim_lisp cell = CAR(rule_);
		if (TRUEP(uim_test0003_wildcard_match(CAR(CAR(cell)), seq_))) {
			for (uim_lisp i_ = CAR(CDR(cell)); CONSP(i_); i_ = CDR(i_)) {
				cands_ = CONS(CONS(CAR(CAR(cell)), CAR(i_)), cands_);
			}
		}
		rule_ = CDR(rule_);
	}
	return uim_scm_callf("reverse", "o", cands_);
}

void
uim_plugin_instance_init(void)
{
	uim_scm_init_proc2("test0003-lib-wildcard-match", uim_test0003_wildcard_match);
	uim_scm_init_proc3("test0003-lib-make-cands-wildcard", uim_test0003_make_cands_wildcard);
}

void
uim_plugin_instance_quit(void)
{
}
