
#include "FallbackContext.h"

FallbackContext::FallbackContext() : QWidget() {
    this->drawingCallback = emptyDrawingCallback;
};

void FallbackContext::paintEvent(QPaintEvent* event) {

    event->accept();

    QPainter painter(this);
    painter.setRenderHints(QPainter::SmoothPixmapTransform, true);
    this->drawingCallback(&painter);
};
