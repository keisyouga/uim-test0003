#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "dynlib.h"

/* stores table file data */
static char *table_buf;

/* read line from string
 * return read bytes
 */
static int
gets_from_string(char *dst, int size, char *src)
{
	int n = 0;
	for (; n < size; n++) {
		dst[n] = src[n];
		if (src[n] == '\0') {
			return n;
		} else if (src[n] == '\n') {
			/* set EOF */
			if (n + 1 < size) {
				n++;
				dst[n] = '\0';
			}
			return n;
		}
	}

	/* n == size */
	return n;
}

/* free table_buf */
static uim_lisp
close_dic()
{
	if (table_buf) {
		free(table_buf);
		table_buf = NULL;
	}
	return uim_scm_t();
}

/* read filename_'s contents to table_buf */
static uim_lisp
open_dic(uim_lisp filename_)
{
	const char *dicfilename = REFER_C_STR(filename_);
	FILE *fp;
	struct stat sb;
	int n;

	/* close opened table */
	close_dic();

	if (stat(dicfilename, &sb) == -1) {
		perror("stat");
		return uim_scm_f();
	}
	table_buf = malloc(sb.st_size + 1);
	if (!table_buf) {
		perror("malloc");
		return uim_scm_f();
	}
	fp = fopen(dicfilename, "r");
	if (!fp) {
		perror("fopen");
		return uim_scm_f();
	}

	n = fread(table_buf, 1, sb.st_size, fp);
	table_buf[n] = '\0';

	fclose(fp);

	return uim_scm_t();
}

/* wildcard match
 * return non-zero if matched
 */
static int
match(const char *stroke, const char *pattern)
{
	while (1) {
		if (!*stroke) {
			while (*pattern == '*') { pattern++; }
			return (*pattern == '\0');
		}
		// not matched
		if (!*pattern) { return 0; }

		if (*pattern == '*') {
			// match test for each character
			return (match(stroke, pattern + 1) || match(stroke + 1, pattern));
		}

		if ((*stroke == *pattern) || (*pattern == '?')) {
			stroke++;
			pattern++;
			continue;
		}
		// not matched
		return 0;
	}
}

/* return non-zero if matched */
static int
my_strcmp(const char *s1, const char *s2)
{
	return strcmp(s1, s2) == 0;
}

/* return non-zero if matched first length of s2 */
static int
my_strncmp(const char *s1, const char *s2)
{
	int len = strlen(s2);
	return strncmp(s1, s2, len) == 0;
}

/* make candidate list '((entry1 . content1) (entry2 . content2) ... )
 */
static uim_lisp
make_cands_internal(const char *query, int cmp(const char *, const char *))
{
	uim_lisp lst_ = uim_scm_null();
	char line[256];
	char entry[256];
	char content[256];
	int nbyte = 0;
	int pos = 0;

	/* somohow, table_buf do not initialized, return empty list */
	if (!table_buf) {
		return lst_;
	}

	while ((nbyte = gets_from_string(line, sizeof(line), &table_buf[pos])) > 0) {
		int n = sscanf(line, "%s %s", entry, content);
		if (n == 2) {
			if (cmp(entry, query)) {
				lst_ = CONS(CONS(MAKE_STR(entry), MAKE_STR(content)), lst_);
			}
		}
		pos += nbyte;
	}

	return uim_scm_callf("reverse", "o", lst_);
}

static uim_lisp
make_cands_wildcard(uim_lisp str_)
{
	const char *query = REFER_C_STR(str_);
	return make_cands_internal(query, match);
}

static uim_lisp
make_cands_wildcard_prefix(uim_lisp str_)
{
	const char *query = REFER_C_STR(str_);
	char query2[256];           /* enough size? */
	sprintf(query2, "%s*", query);
	return make_cands_internal(query2, match);
}

static uim_lisp
make_cands_find(uim_lisp str_)
{
	const char *query = REFER_C_STR(str_);
	return make_cands_internal(query, my_strcmp);
}

static uim_lisp
make_cands_find_prefix(uim_lisp str_)
{
	const char *query = REFER_C_STR(str_);
	return make_cands_internal(query, my_strncmp);
}

void
uim_plugin_instance_init(void)
{
	uim_scm_init_proc1("test0003-lib-open-dic", open_dic);
	uim_scm_init_proc0("test0003-lib-close-dic", close_dic);
	uim_scm_init_proc1("test0003-lib-make-cands-wildcard-prefix", make_cands_wildcard_prefix);
	uim_scm_init_proc1("test0003-lib-make-cands-wildcard", make_cands_wildcard);
	uim_scm_init_proc1("test0003-lib-make-cands-find-prefix", make_cands_find_prefix);
	uim_scm_init_proc1("test0003-lib-make-cands-find", make_cands_find);
}

void
uim_plugin_instance_quit(void)
{
}
