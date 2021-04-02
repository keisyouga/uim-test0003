
# path to uim.h
UIM_DIR = ../uim/uim
CFLAGS = -I$(UIM_DIR) -Wall -g -Wextra -Werror
# where libuim.so
LIBS = $(UIM_DIR)/.libs/libuim.so

libuim-switchtable.so: switchtable.o
	$(CC) $(CFLAGS) -shared -o $@ $< $(LIBS)

clean:
	$(RM) libuim-switchtable.so switchtable.o
