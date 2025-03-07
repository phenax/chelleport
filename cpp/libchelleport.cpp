#include <leptonica/allheaders.h>
#include <memory>
#include <ostream>
#include <tesseract/baseapi.h>
#include <thread>
#include <vector>

#include "../include/image.h"
#include "../include/libchelleport.h"
#include "../include/recognizer.h"

extern "C" OCRMatch *findWordCoordinates(const char *image_path, int *size) {
  OCRMatchSet matches;
  MEASURE("OCR", { matches = extractTextMatches(image_path); });

  OCRMatch *ptr = new OCRMatch[matches.size()];
  std::copy(matches.begin(), matches.end(), ptr);

  *size = matches.size();
  std::cout << "Match count: " << *size << std::endl;

  return ptr;
}

OCRMatchSet extractTextMatches(const char *imagePath) {
  Pix *image = image::loadImage(imagePath);

  if (image == nullptr) {
    return OCRMatchSet();
  }

  // printf("imagePath: %s\n", imagePath);
  // pixWrite(imagePath, image, IFF_JFIF_JPEG);

  int width = pixGetWidth(image);
  int height = pixGetHeight(image);

  std::vector<std::unique_ptr<Recognizer>> recognizers;

  // clang-format off
  recognizers.push_back(
    std::make_unique<Recognizer>("top-left",      0,          0,           width / 2,  height / 2));
  recognizers.push_back(
    std::make_unique<Recognizer>("top-right",     width / 2,  0,           width / 2,  height / 2));
  recognizers.push_back(
    std::make_unique<Recognizer>("bottom-left",   0,          height / 2,  width / 2,  height / 2));
  recognizers.push_back(
    std::make_unique<Recognizer>("bottom-right",  width / 2,  height / 2,  width / 2,  height / 2));
  // clang-format on

  return runRecognizers(recognizers, image);
}

OCRMatchSet
runRecognizers(std::vector<std::unique_ptr<Recognizer>> &recognizers,
               Pix *image) {
  OCRMatchSet results;
  std::shared_ptr<Pix> sharedImage(image, [](Pix *p) { pixDestroy(&p); });

  std::vector<std::thread> workers;
  workers.reserve(recognizers.size());

  for (auto &recognizer : recognizers) {
    workers.push_back(std::thread([&recognizer, &sharedImage]() {
      MEASURE(recognizer->id, { recognizer->recognize(sharedImage.get()); })
    }));
  }

  for (std::thread &t : workers) {
    if (t.joinable())
      t.join();
  }

  for (auto &recognizer : recognizers) {
    for (auto &match : recognizer->getResults())
      results.insert(match);
  }

  return results;
}
