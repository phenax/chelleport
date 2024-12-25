#include <tesseract/publictypes.h>
#include <vector>

// NOTE: Remember to update size and alignment in ocr hs module on change
struct OCRMatch {
  int startX, startY;
  int endX, endY;
  const char *text;
};

#define CONFIDENCE_THRESHOLD 25.
#define MIN_CHARACTER_COUNT 2

const tesseract::PageIteratorLevel RESULT_ITER_MODE = tesseract::RIL_WORD;

extern "C" {
OCRMatch *findWordCoordinates(const char *image_path, /* returns */ int *size);
}

std::vector<OCRMatch> extractTextCoordinates(const char *imagePath);

void showMatch(const OCRMatch &match);
