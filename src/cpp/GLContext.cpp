
#include "utils.h"
#include "MainWindow.h"


GLContext::GLContext(const QGLFormat& format) : QGLWidget(format) {
};

void GLContext::paintEvent(QPaintEvent* event) {
    event->accept();

    QPainter painter(this);
    painter.setRenderHints(QPainter::SmoothPixmapTransform, true);
    this->drawingCallback(&painter);
};


// the postGUI signal-slot-pair is necessary to execute a command in the
// GUI thread.
void GLContext::postGUI(guiAction* action) {
    emit postGUISignal(action);
};

void GLContext::postGUISlot(guiAction* action) {
    action();
};
