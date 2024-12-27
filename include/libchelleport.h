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

std::vector<OCRMatch> extractTextMatches(const char *imagePath);

std::vector<OCRMatch>
runRecognizers(std::vector<std::unique_ptr<Recognizer>> &recognizers,
               Pix *image);
