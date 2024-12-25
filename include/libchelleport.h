#include <leptonica/allheaders.h>
#include <tesseract/publictypes.h>
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

std::vector<OCRMatch> extractTextCoordinates(const char *imagePath);

void showMatch(const OCRMatch &match);

void preprocessImage(Pix **image);
