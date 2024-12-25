#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <leptonica/allheaders.h>
#include <tesseract/baseapi.h>
#include <tesseract/publictypes.h>
#include <vector>

#include "../include/libchelleport.h"

OCRMatch *findWordCoordinates(const char *image_path, int *size) {
  auto matches = extractTextCoordinates(image_path);

  static OCRMatch *ptr = new OCRMatch[matches.size()];
  std::copy(matches.begin(), matches.end(), ptr);

  // for (const auto &match : matches)
  //   showMatch(match);

  printf("Count: %ld\n", matches.size());

  *size = matches.size();
  return ptr;
}

std::vector<OCRMatch> extractTextCoordinates(const char *imagePath) {
  std::vector<OCRMatch> results;
  tesseract::TessBaseAPI *tesseract = new tesseract::TessBaseAPI();

  if (tesseract->Init(nullptr, "eng")) {
    std::cerr << "Could not initialize tesseract." << std::endl;
    return results;
  }

  Pix *image = pixRead(imagePath);
  if (!image) {
    std::cerr << "Could not load image " << imagePath << std::endl;
    return results;
  }

  preprocessImage(&image);

  // printf("imagePath: %s\n", imagePath);
  // pixWrite(imagePath, image, IFF_JFIF_JPEG);

  tesseract->SetImage(image);
  tesseract->Recognize(0);

  tesseract::ResultIterator *iterator = tesseract->GetIterator();
  auto level = RESULT_ITER_MODE;

  if (iterator != 0) {
    do {
      float conf = iterator->Confidence(level);
      const char *word = iterator->GetUTF8Text(level);

      if (conf > CONFIDENCE_THRESHOLD && word != nullptr &&
          strlen(word) >= MIN_CHARACTER_COUNT) {
        int x1, y1, x2, y2;
        iterator->BoundingBox(level, &x1, &y1, &x2, &y2);
        results.push_back(
            OCRMatch{(int)(x1 / scaleFactor), (int)(y1 / scaleFactor),
                     (int)(x2 / scaleFactor), (int)(y2 / scaleFactor), word});
      }
    } while (iterator->Next(level));
  }

  delete iterator;
  tesseract->End();
  delete tesseract;
  pixDestroy(&image);

  return results;
}

void preprocessImage(Pix **image) {
  Pix *temp;

  // Scale
  if (scaleFactor != 1) {
    temp = pixScale(*image, scaleFactor, scaleFactor);
    pixDestroy(image);
    *image = temp;
  }

  // Grayscale
  if (pixGetDepth(*image) > 8) {
    temp = pixConvertRGBToGray(*image, grayscaleWeightRed, grayscaleWeightGreen,
                               grayscaleWeightBlue);
    pixDestroy(image);
    *image = temp;
  }

  // Contrast
  pixContrastTRC(*image, *image, contrast);

  // Sharpness
  // temp = pixUnsharpMaskingGrayFast(*image, 1, sharpness, 1);
  temp = pixUnsharpMasking(*image, 1, sharpness);
  pixDestroy(image);
  *image = temp;
}

void showMatch(const OCRMatch &match) {
  std::cout << "Text: " << match.text << "; Position: (" << match.startX << ","
            << match.startY << ") -> (" << match.endX << "," << match.endY
            << ")" << "\n\n";
}
