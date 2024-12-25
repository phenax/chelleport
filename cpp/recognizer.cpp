#include <leptonica/allheaders.h>
#include <tesseract/baseapi.h>

#include "../include/recognizer.h"

void Recognizer::initializeTesseract() {
  tesseract = new tesseract::TessBaseAPI();
  tesseract->SetPageSegMode(tesseract::PSM_AUTO);

  if (tesseract->Init(nullptr, "eng", tesseract::OEM_LSTM_ONLY))
    fail("Could not initialize tesseract.");
}

void Recognizer::recognize(Pix *image) {
  if (failed)
    return;

  tesseract->SetImage(image);
  tesseract->SetRectangle(x, y, width, height);
  if (tesseract->Recognize(0) != 0)
    fail("tesseract recognize failed");
}

std::vector<OCRMatch> Recognizer::getResults() {
  std::vector<OCRMatch> results;

  if (failed)
    return results;

  tesseract::ResultIterator *iterator = tesseract->GetIterator();
  if (iterator == 0)
    return results;

  do {
    auto match = fetchMatch(iterator);
    if (match != nullptr)
      results.push_back(*match);
  } while (iterator->Next(ITER_LEVEL));

  delete iterator;

  return results;
}

OCRMatch *Recognizer::fetchMatch(tesseract::ResultIterator *iterator) {
  if (iterator->Confidence(ITER_LEVEL) < CONFIDENCE_THRESHOLD)
    return nullptr;

  const char *word = iterator->GetUTF8Text(ITER_LEVEL);

  if (word == nullptr || strlen(word) < MIN_CHARACTER_COUNT)
    return nullptr;

  int x1, y1, x2, y2;
  iterator->BoundingBox(ITER_LEVEL, &x1, &y1, &x2, &y2);

  return new OCRMatch(
      {(int)(x1 / image::scaleFactor), (int)(y1 / image::scaleFactor),
       (int)(x2 / image::scaleFactor), (int)(y2 / image::scaleFactor), word});
}

void Recognizer::fail(const char *msg) {
  this->failed = true;
  std::cerr << msg << std::endl;
}
