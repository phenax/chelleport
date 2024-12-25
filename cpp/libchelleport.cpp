#include <leptonica/allheaders.h>
#include <memory>
#include <ostream>
#include <tesseract/baseapi.h>
#include <thread>
#include <vector>

#include "../include/image.h"
#include "../include/libchelleport.h"
#include "../include/recognizer.h"

extern "C" {
OCRMatch *findWordCoordinates(const char *image_path, int *size) {
  std::vector<OCRMatch> matches;
  MEASURE("OCR", { matches = extractTextMatches(image_path); });

  std::cout << "Match count: " << matches.size() << std::endl;

  static OCRMatch *ptr = new OCRMatch[matches.size()];
  std::copy(matches.begin(), matches.end(), ptr);

  *size = matches.size();
  return ptr;
}
}

std::vector<OCRMatch> extractTextMatches(const char *imagePath) {
  std::vector<OCRMatch> results;

  Pix *image = image::loadImage(imagePath);
  if (image == nullptr)
    return results;

  // printf("imagePath: %s\n", imagePath);
  // pixWrite(imagePath, image, IFF_JFIF_JPEG);

  int width = pixGetWidth(image);
  int height = pixGetHeight(image);

  std::vector<std::unique_ptr<Recognizer>> recognizers;
  recognizers.push_back(
      std::make_unique<Recognizer>(0, 0, width / 2, height / 2));

  recognizers.push_back(
      std::make_unique<Recognizer>(width / 2, 0, width / 2, height / 2));

  recognizers.push_back(
      std::make_unique<Recognizer>(0, height / 2, width / 2, height / 2));

  recognizers.push_back(std::make_unique<Recognizer>(width / 2, height / 2,
                                                     width / 2, height / 2));

  return runRecognizers(recognizers, image);
}

std::vector<OCRMatch>
runRecognizers(std::vector<std::unique_ptr<Recognizer>> &recognizers,
               Pix *image) {
  std::vector<OCRMatch> results;
  std::shared_ptr<Pix> sharedImage(image, [](Pix *p) { pixDestroy(&p); });

  std::vector<std::thread> workers;
  workers.reserve(recognizers.size());

  for (auto &ext : recognizers) {
    workers.push_back(std::thread(
        [&ext, &sharedImage]() { ext->recognize(sharedImage.get()); }));
  }

  for (std::thread &t : workers) {
    if (t.joinable())
      t.join();
  }

  for (auto &ext : recognizers) {
    for (auto &match : ext->getResults())
      results.push_back(match);
  }

  return results;
}
