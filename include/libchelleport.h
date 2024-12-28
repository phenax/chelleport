#pragma once
#include <chrono>
#include <iostream>
#include <leptonica/allheaders.h>
#include <memory>
#include <tesseract/baseapi.h>
#include <vector>

#include "./recognizer.h"

#define MEASURE(label, stmts)                                                  \
  auto start = std::chrono::high_resolution_clock::now();                      \
  stmts;                                                                       \
  auto end = std::chrono::high_resolution_clock::now();                        \
  auto duration =                                                              \
      std::chrono::duration_cast<std::chrono::microseconds>(end - start);      \
  std::cout << label << ": " << duration.count() / 1000.0 << " ms" << std::endl;

extern "C" OCRMatch *findWordCoordinates(const char *image_path,
                                         /* returns */ int *size);

struct ScreenPositionComparator {
  bool operator()(const OCRMatch &a, const OCRMatch &b) const {
    if (abs(a.startY - b.startY) < 5)
      return a.startX < b.startX;
    return a.startY < b.startY;
  }
};

typedef std::set<OCRMatch, ScreenPositionComparator> OCRMatchSet;

OCRMatchSet extractTextMatches(const char *imagePath);

OCRMatchSet
runRecognizers(std::vector<std::unique_ptr<Recognizer>> &recognizers,
               Pix *image);
