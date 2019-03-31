SRCS=ioctl.c
CC=gcc
TARGET=ioctl.so

$(TARGET):$(SRCS)
	$(CC) -g -fPIC -shared -Wall $(SRCS) -o $(TARGET)

clean:
	rm -f $(TARGET)
