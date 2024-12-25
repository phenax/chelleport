#pragma once
#include <leptonica/allheaders.h>

namespace image {
// Preprocessing configuration
static const float contrast = 0.3;
static const float sharpness = 0.7;
static const float scaleFactor = 1;
static const float grayscaleWeightRed = 0.114;
static const float grayscaleWeightGreen = 0.587;
static const float grayscaleWeightBlue = 0.299;

Pix *loadImage(const char *imagePath);
} // namespace image

#define INLINE_IMAGE_PROC(process)                                             \
  temp = process;                                                              \
  pixDestroy(image);                                                           \
  *image = temp;
