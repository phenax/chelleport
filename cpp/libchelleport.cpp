#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <leptonica/allheaders.h>
#include <tesseract/baseapi.h>
#include <vector>

#include "../include/libchelleport.h"

OCRMatch *findWordCoordinates(const char *image_path, int *size) {
  std::vector<OCRMatch> matches;
  MEASURE("OCR", { matches = extractTextCoordinates(image_path); });

  std::cout << "Word count: " << matches.size() << std::endl;

  static OCRMatch *ptr = new OCRMatch[matches.size()];
  std::copy(matches.begin(), matches.end(), ptr);

  *size = matches.size();
  return ptr;
}

std::vector<OCRMatch> extractTextCoordinates(const char *imagePath) {
  std::vector<OCRMatch> results;

  auto tesseract = initializeTesseract();
  if (tesseract == nullptr)
    return results;

  Pix *image = loadImage(imagePath);
  if (image == nullptr)
    return results;

  // printf("imagePath: %s\n", imagePath);
  // pixWrite(imagePath, image, IFF_JFIF_JPEG);

  tesseract->SetImage(image);
  tesseract->Recognize(0);

  tesseract::ResultIterator *iterator = tesseract->GetIterator();
  auto level = RESULT_ITER_MODE;

  if (iterator != 0) {
    do {
      if (iterator->Confidence(level) > CONFIDENCE_THRESHOLD) {
        const char *word = iterator->GetUTF8Text(level);

        if (word != nullptr && strlen(word) >= MIN_CHARACTER_COUNT) {
          int x1, y1, x2, y2;
          iterator->BoundingBox(level, &x1, &y1, &x2, &y2);
          OCRMatch match({(int)(x1 / scaleFactor), (int)(y1 / scaleFactor),
                          (int)(x2 / scaleFactor), (int)(y2 / scaleFactor),
                          word});
          results.push_back(match);
        }
      }
    } while (iterator->Next(level));
  }

  delete iterator;
  tesseract->End();
  delete tesseract;
  pixDestroy(&image);

  return results;
}

inline tesseract::TessBaseAPI *initializeTesseract() {
  auto *tesseract = new tesseract::TessBaseAPI();
  tesseract->SetPageSegMode(tesseract::PSM_AUTO);

  if (tesseract->Init(nullptr, "eng", tesseract::OEM_LSTM_ONLY)) {
    std::cerr << "Could not initialize tesseract." << std::endl;
    return nullptr;
  }

  return tesseract;
}

inline Pix *loadImage(const char *imagePath) {
  Pix *image = pixRead(imagePath);
  if (!image) {
    std::cerr << "Could not load image " << imagePath << std::endl;
    return nullptr;
  }

  preprocessImage(&image);

  return image;
}

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

void printMatch(const OCRMatch &match) {
  std::cout << "Text: " << match.text << "; Position: (" << match.startX << ","
            << match.startY << ") -> (" << match.endX << "," << match.endY
            << ")" << std::endl
            << std::endl;
}
