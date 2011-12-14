
#include "utils.h"
#include "MainWindow.h"


GLContext::GLContext(const QGLFormat& format) : QGLWidget(format) {
    this->drawingCallback = emptyDrawingCallback;
};

void GLContext::paintEvent(QPaintEvent* event) {

    event->accept();

    QPainter painter(this);
    painter.setRenderHints(QPainter::SmoothPixmapTransform, true);
    this->drawingCallback(&painter);
};
