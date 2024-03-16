#include <stdio.h>
#include "doggo.h"

// static const char* BreedToString[4] = {
//   "Labrador",
//   "Golden Retriever",
//   "Pug",
//   "Poodle"
// };

// void eleven_out_of_ten_majestic_af(Doggo* pupper) {
//   printf("doggo says %d\n", pupper->many);
//   printf("doggo is a %s\n", BreedToString[pupper->breed]);
//   printf("doggo weighs %.1fkg\n", pupper->weight);
// }

// void no_input_no_output(void) {
//   printf("We are doing nothing (of importance)\n");
// }

void print_age(int* age) {
  printf("Age: %d\n", *age);
}
// void print_name(char* name) {
//   printf("Name: %s\n", name);
// }
// void print_doggo(Doggo *dog) {
//   printf("wow: %c\n", dog->wow);
//   print_age(&dog->many);
// }
