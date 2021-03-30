
# path to uim.h
UIM_DIR = ../uim/uim
CFLAGS = -I$(UIM_DIR) -Wall -g -Wextra -Werror
# where libuim.so
LIBS = $(UIM_DIR)/.libs/libuim.so

libuim-test0003.so: test0003.o
	$(CC) $(CFLAGS) -shared -o $@ $< $(LIBS)

clean:
	$(RM) libuim-test0003.so test0003.o
