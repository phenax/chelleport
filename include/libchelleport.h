#include <string>

// NOTE: Remember to update size and alignment in ocr hs module on change
struct OCRMatch {
  int startX, startY;
  int endX, endY;
  const char *text;
};

extern "C" {
OCRMatch *findWordCoordinates(const char *image_path, /* returns */ int *size);
}
