
#include "utils.h"
#include "MainWindow.h"


GLContext::GLContext(const QGLFormat& format) : QGLWidget(format) {
    this->drawingCallback = emptyDrawingCallback;
    this->oldDrawingCallback = emptyDrawingCallback;
};

void GLContext::setDrawingCallback(drawingCallbackFunction* dcb) {
    this->drawingCallback = dcb;
};

void GLContext::paintEvent(QPaintEvent* event) {

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
