#include <iostream>
#include <leptonica/allheaders.h>
#include <tesseract/baseapi.h>

#include "../include/image.h"

namespace image {
void preprocessImage(Pix **image) {
  Pix *temp;

  // Scale
  if (scaleFactor != 1) {
    INLINE_IMAGE_PROC(pixScale(*image, scaleFactor, scaleFactor));
  }

  // Grayscale
  if (pixGetDepth(*image) > 8) {
    INLINE_IMAGE_PROC(pixConvertRGBToGray(
        *image, grayscaleWeightRed, grayscaleWeightGreen, grayscaleWeightBlue));
  }

  // Contrast
  pixContrastTRC(*image, *image, contrast);

  // Sharpness
  // INLINE_IMAGE_PROC(pixUnsharpMaskingGrayFast(*image, 1, sharpness, 1));
  INLINE_IMAGE_PROC(pixUnsharpMasking(*image, 1, sharpness));
}

Pix *loadImage(const char *imagePath) {
  Pix *image = pixRead(imagePath);
  if (!image) {
    std::cerr << "Could not load image " << imagePath << std::endl;
    return nullptr;
  }

  preprocessImage(&image);

  return image;
}
} // namespace image
