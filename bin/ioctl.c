#include <sys/ioctl.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>



struct winsize wbuf;

void set_screen_dimensions(int row, int col)
{
  struct winsize wbuf;
  wbuf.ws_row = row;
  wbuf.ws_col = col;
  int fd = open("/dev/tty1", 0);

  ioctl(fd, TIOCSWINSZ, &wbuf);
  close(fd);
}

void get_screen_dimensions()
{
  int fd = open("/dev/tty1", 0);

  ioctl(fd, TIOCGWINSZ, &wbuf);
  printf("%d - %d\n", wbuf.ws_row, wbuf.ws_col);
}

int getx()
{
  return wbuf.ws_row;
}

int gety()
{
  return wbuf.ws_col;
}

/*
void main()
{
  struct winsize wbuf = get_screen_dimensions();
  printf("%d - %d", wbuf.ws_row, wbuf.ws_col);
  set_screen_dimensions();
  wbuf = get_screen_dimensions();
  printf("%d - %d", wbuf.ws_row, wbuf.ws_col);
}
*/
