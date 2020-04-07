#include <bcm2835.h>
#include "bcm.h"

#define CS 8
#define HRDY 24
#define RESET 17

void waitForBus()
{
  while(!bcm2835_gpio_lev(HRDY));
}

void openBus()
{
  waitForBus();
  bcm2835_gpio_write(CS, LOW);
}

void closeBus()
{
  bcm2835_gpio_write(CS, HIGH);
}

int transfer(int value)
{
  value = value & 0xFFFF;
  int retVal = 0x0000;
  retVal |= bcm2835_spi_transfer(value >> 8) << 8;
  retVal |= bcm2835_spi_transfer(value);
  return retVal;
}

bool initBCM()
{
  if (!bcm2835_init()) {
    return false;
  }

  bcm2835_spi_begin();
  bcm2835_spi_setBitOrder(BCM2835_SPI_BIT_ORDER_MSBFIRST);      //default
  bcm2835_spi_setDataMode(BCM2835_SPI_MODE0);                   //default
  bcm2835_spi_setClockDivider(BCM2835_SPI_CLOCK_DIVIDER_32);		//default

  bcm2835_gpio_fsel(CS, BCM2835_GPIO_FSEL_OUTP);
  bcm2835_gpio_fsel(HRDY, BCM2835_GPIO_FSEL_INPT);
  bcm2835_gpio_fsel(RESET, BCM2835_GPIO_FSEL_OUTP);

  bcm2835_gpio_write(CS, HIGH);

  bcm2835_gpio_write(RESET, LOW);
  bcm2835_delay(100);
  bcm2835_gpio_write(RESET, HIGH);
  return true;
}

void freeBCM()
{
  bcm2835_spi_end();
  bcm2835_close();
}
