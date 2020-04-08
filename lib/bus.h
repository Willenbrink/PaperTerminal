#include <stdint.h>
#include <stdbool.h>

/* Uses the bcm2835 library to interface via SPI.
 * https://www.airspayce.com/mikem/bcm2835/index.html
 */

// Call once
bool initBCM();

// Call once
void freeBCM();

// Wait until the bus is ready to transfer data
// TODO Verify the significance of this
void waitForBus();

// Prepare for data transfer
void openBus();

// End data transfer
void closeBus();

// Send and receive data simultaneously
// TODO check why we dont use uint8_t instead as the SPI transfers one byte at once.
uint16_t transfer(uint16_t value);
