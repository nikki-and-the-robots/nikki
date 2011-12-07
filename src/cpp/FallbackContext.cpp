
#include "FallbackContext.h"

FallbackContext::FallbackContext() : QWidget() {
    this->drawingCallback = emptyDrawingCallback;
    this->oldDrawingCallback = emptyDrawingCallback;
};

void FallbackContext::setDrawingCallback(drawingCallbackFunction* dcb) {
    this->drawingCallback = dcb;
};

void FallbackContext::paintEvent(QPaintEvent* event) {

    // free old callback
    if (this->drawingCallback != this->oldDrawingCallback)
        if (this->oldDrawingCallback != emptyDrawingCallback)
            freeDrawingCallback(this->oldDrawingCallback);
    this->oldDrawingCallback = this->drawingCallback;


    event->accept();

    QPainter painter(this);
    painter.setRenderHints(QPainter::SmoothPixmapTransform, true);
    this->drawingCallback(&painter);
};
