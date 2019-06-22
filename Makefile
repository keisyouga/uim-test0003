CFLAGS = -I../uim/uim -Wall -g -Wextra -Werror

libuim-test0003.so: test0003.o
	$(CC) $(CFLAGS) -shared -o $@ $<
