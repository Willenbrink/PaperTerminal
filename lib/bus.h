#include <stdint.h>
#include <stdbool.h>

/*
 * First level of abstraction:
 * Interact with the bus.
 * Transfer bytes to and from controller
 * Uses the bcm2835 library to interface via SPI.
 * https://www.airspayce.com/mikem/bcm2835/index.html
 */

bool initBCM(); //Call once
void freeBCM();
void waitForBus(); //Wait until bus is ready TODO check significance of this?
void openBus(); //Prepare for datatransfer
void closeBus(); //End datatransfer
// TODO check why we dont use uint8_t instead as the SPI transfers one byte at once.
uint16_t transfer(uint16_t value); //Send and receive data simultaneously
