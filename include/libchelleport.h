#include <chrono>
#include <leptonica/allheaders.h>
#include <tesseract/baseapi.h>
#include <vector>

// NOTE: Remember to update size and alignment in ocr hs module on change
struct OCRMatch {
  int startX, startY;
  int endX, endY;
  const char *text;
};

// OCR configuration
#define CONFIDENCE_THRESHOLD 25.
#define MIN_CHARACTER_COUNT 3
const tesseract::PageIteratorLevel RESULT_ITER_MODE = tesseract::RIL_WORD;

// Preprocessing configuration
const float contrast = 0.3;
const float sharpness = 0.7;
const float scaleFactor = 1;
const float grayscaleWeightRed = 0.114;
const float grayscaleWeightGreen = 0.587;
const float grayscaleWeightBlue = 0.299;

extern "C" {
OCRMatch *findWordCoordinates(const char *image_path, /* returns */ int *size);
}

tesseract::TessBaseAPI *initializeTesseract();

Pix *loadImage(const char *imagePath);

std::vector<OCRMatch> extractTextCoordinates(const char *imagePath);

void printMatch(const OCRMatch &match);

void preprocessImage(Pix **image);

#define INLINE_IMAGE_PROC(process)                                             \
  temp = process;                                                              \
  pixDestroy(image);                                                           \
  *image = temp;

#define MEASURE(label, stmts)                                                  \
  auto start = std::chrono::high_resolution_clock::now();                      \
  stmts;                                                                       \
  auto end = std::chrono::high_resolution_clock::now();                        \
  auto duration =                                                              \
      std::chrono::duration_cast<std::chrono::microseconds>(end - start);      \
  std::cout << label << ": " << duration.count() / 1000.0 << " ms" << std::endl;
