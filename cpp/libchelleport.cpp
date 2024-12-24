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

std::vector<OCRMatch> extractTextCoordinates(const char *imagePath);

#define CONFIDENCE_THRESHOLD 30.
#define MIN_CHARACTER_COUNT 2
const tesseract::PageIteratorLevel RESULT_ITER_MODE = tesseract::RIL_WORD;

OCRMatch *findWordCoordinates(const char *image_path, int *size) {
  auto boxes = extractTextCoordinates(image_path);
  static OCRMatch *ptr = new OCRMatch[boxes.size()];
  std::copy(boxes.begin(), boxes.end(), ptr);

  // for (const auto &box : boxes) {
  //   std::cout << box.text << "\n\n";
  //   std::cout << "Text: " << box.text << "\nPosition: (" << box.startX << ","
  //             << box.startY << ") -> (" << box.endX << "," << box.endY << ")"
  //             << "\n\n";
  // }

  *size = boxes.size();
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

  tesseract->SetImage(image);
  tesseract->Recognize(0);

  tesseract::ResultIterator *iterator = tesseract->GetIterator();
  auto level = RESULT_ITER_MODE;

  if (iterator != 0) {
    do {
      float conf = iterator->Confidence(level);
      const char *word = iterator->GetUTF8Text(level);
      int x1, y1, x2, y2;
      iterator->BoundingBox(level, &x1, &y1, &x2, &y2);

      if (conf > CONFIDENCE_THRESHOLD && word != nullptr &&
          strlen(word) >= MIN_CHARACTER_COUNT) {
        results.push_back(OCRMatch{x1, y1, x2, y2, word});
      }
    } while (iterator->Next(level));
  }

  delete iterator;
  tesseract->End();
  delete tesseract;
  pixDestroy(&image);

  return results;
}
