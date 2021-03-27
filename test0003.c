#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "dynlib.h"

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
			dst[n] = '\0';
			return n + 1; /* return number of bytes read, not a dst's length */
		}
	}

	/* n == size */
	return n;
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
make_cands_internal(uim_lisp query_, uim_lisp table_, int cmp(const char *, const char *))
{
	const char *query = REFER_C_STR(query_);
	const char *tablefilename = REFER_C_STR(table_);
	uim_lisp lst_ = uim_scm_null();
	char line[256];
	char entry[256];
	char content[256];
	int nbyte = 0;
	int pos = 0;

	/* open, mmap, make candidate list, munmap, close */
	int fd;
	struct stat sb;
	char *addr;

	fd = open(tablefilename, O_RDONLY);
	if (fd == -1) {
		perror("open");
		return lst_;
	}

	if (fstat(fd, &sb) == -1) {
		perror("fstat");
		return lst_;
	}

	addr = mmap(NULL, sb.st_size, PROT_READ, MAP_SHARED, fd, 0);
	if (addr == MAP_FAILED) {
		perror("mmap");
		return lst_;
	}

	while ((nbyte = gets_from_string(line, sizeof(line), &addr[pos])) > 0) {
		/* be careful about buffer overflow */
		int n = sscanf(line, "%255s %255[^\r\n]", entry, content);
		if (n == 2) {
			if (cmp(entry, query)) {
				lst_ = CONS(CONS(MAKE_STR(entry), MAKE_STR(content)), lst_);
			}
		}
		pos += nbyte;
	}

	if (munmap(addr, sb.st_size) == -1) {
		perror("munmap");
	}
	close(fd);

	return uim_scm_callf("reverse", "o", lst_);
}

static uim_lisp
make_cands_wildcard(uim_lisp str_, uim_lisp table_)
{
	return make_cands_internal(str_, table_, match);
}

static uim_lisp
make_cands_wildcard_prefix(uim_lisp str_, uim_lisp table_)
{
	/* "str" => "str*" */
	uim_lisp str2_ = uim_scm_callf("string-append", "oo", str_, MAKE_STR("*"));
	return make_cands_internal(str2_, table_, match);
}

static uim_lisp
make_cands_find(uim_lisp str_, uim_lisp table_)
{
	return make_cands_internal(str_, table_, my_strcmp);
}

static uim_lisp
make_cands_find_prefix(uim_lisp str_, uim_lisp table_)
{
	return make_cands_internal(str_, table_, my_strncmp);
}

void
uim_plugin_instance_init(void)
{
	uim_scm_init_proc2("test0003-lib-make-cands-wildcard-prefix", make_cands_wildcard_prefix);
	uim_scm_init_proc2("test0003-lib-make-cands-wildcard", make_cands_wildcard);
	uim_scm_init_proc2("test0003-lib-make-cands-find-prefix", make_cands_find_prefix);
	uim_scm_init_proc2("test0003-lib-make-cands-find", make_cands_find);
}

void
uim_plugin_instance_quit(void)
{
}
