#include <stdio.h>
#include "doggo.h"

static const char* BreedToString[3] = {
  "Labrador",
  "Golden Retriever",
  "Pug",
};

void eleven_out_of_ten_majestic_af(Doggo* pupper) {
  printf("doggo says %d\n", pupper->many);
  printf("doggo is a %s\n", BreedToString[pupper->breed]);
}

