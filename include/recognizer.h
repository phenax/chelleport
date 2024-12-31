#pragma once
#include <iostream>
#include <leptonica/allheaders.h>
#include <tesseract/baseapi.h>

#include "./image.h"

// OCR configuration
#define CONFIDENCE_THRESHOLD 20.
#define MIN_CHARACTER_COUNT 3
const tesseract::PageIteratorLevel ITER_LEVEL = tesseract::RIL_WORD;

// NOTE: Remember to update size and alignment in hs type on change
struct OCRMatch {
  int startX, startY;
  int endX, endY;
  const char *text;
};

class Recognizer {
  tesseract::TessBaseAPI *tesseract;
  int x, y, width, height;
  bool failed = false;

public:
  const char *id;

  Recognizer(const char *id, int x, int y, int width, int height)
      : id(id), x(x), y(y), width(width), height(height) {
    initializeTesseract();
  }

  ~Recognizer() { tesseract->End(); }

  void fail(const char *msg);

  void recognize(Pix *image);

  OCRMatch *fetchMatch(tesseract::ResultIterator *iterator);

  std::vector<OCRMatch> getResults();

private:
  void initializeTesseract();
};

inline void printMatch(const OCRMatch &match) {
  std::cout << "Text: " << match.text << "; Position: (" << match.startX << ","
            << match.startY << ") -> (" << match.endX << "," << match.endY
            << ")" << std::endl
            << std::endl;
}
