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

  tesseract->SetImage(image);
  tesseract->Recognize(0);

  tesseract::ResultIterator *iterator = tesseract->GetIterator();
  auto level = RESULT_ITER_MODE;
  int x1, y1, x2, y2;

  if (iterator != 0) {
    do {
      float conf = iterator->Confidence(level);
      const char *word = iterator->GetUTF8Text(level);

      if (conf > CONFIDENCE_THRESHOLD && word != nullptr &&
          strlen(word) >= MIN_CHARACTER_COUNT) {
        iterator->BoundingBox(level, &x1, &y1, &x2, &y2);
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

void showMatch(const OCRMatch &match) {
  std::cout << "Text: " << match.text << "; Position: (" << match.startX << ","
            << match.startY << ") -> (" << match.endX << "," << match.endY
            << ")" << "\n\n";
}
