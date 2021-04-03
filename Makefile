
# path to uim.h
UIM_DIR = /usr/include/uim

# where libuim.so
# do not need to link with libuim.so ?
#LIBS = /usr/lib/x86_64-linux-gnu/libuim.so.8

libuim-switchtable.so: switchtable.o
	$(CC) $(CFLAGS) -shared -o $@ $< $(LIBS)

clean:
	$(RM) libuim-switchtable.so switchtable.o
