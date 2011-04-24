
#include "utils.h"
#include "MainWindow.h"


void emptyDrawingCallback(QPainter* ptr) {
};

GLContext::GLContext(const QGLFormat& format) : QGLWidget(format) {
    mainWindow = new MainWindow();
    ((MainWindow*) mainWindow)->setChild(this);

    drawingCallback = emptyDrawingCallback;
    keyCallback = NULL;
    arrowAutoRepeat = true;

    this->setAutoFillBackground(false);
    this->setCursor(Qt::BlankCursor);
    this->setFocusPolicy(Qt::StrongFocus);

    this->repaintTimer = new QTimer(this);
    QObject::connect(this->repaintTimer, SIGNAL(timeout()), this, SLOT(update()));

    QObject::connect(this, SIGNAL(postGUISignal(guiAction*)), this, SLOT(postGUISlot(guiAction*)));
};

void GLContext::paintEvent(QPaintEvent* event) {
    event->accept();

    QPainter painter(this);
    painter.setRenderHints(QPainter::SmoothPixmapTransform, true);
    this->drawingCallback(&painter);
};

void GLContext::keyPressEvent(QKeyEvent* e) {
    if (((this->arrowAutoRepeat && isArrowKey(e)) || (! e->isAutoRepeat())) && (keyCallback != NULL)) {
        e->accept();
        this->keyCallback(0, e);
    }
};

void GLContext::keyReleaseEvent(QKeyEvent* e) {
    if ((! e->isAutoRepeat()) && (keyCallback != NULL)) {
        e->accept();
        this->keyCallback(1, e);
    }
};

void GLContext::focusOutEvent(QFocusEvent* e) {
    if (keyCallback != NULL) {
        this->keyCallback(2, NULL);
    }
};

void GLContext::closeEvent(QCloseEvent* e) {
    e->ignore();
    if (keyCallback != NULL) {
        this->keyCallback(3, NULL);
    }
};

// the postGUI signal-slot-pair is necessary to execute a command in the
// GUI thread.
void GLContext::postGUI(guiAction* action) {
    emit postGUISignal(action);
};

void GLContext::postGUISlot(guiAction* action) {
    action();
};


// * external C

extern "C" GLContext* newGLContext(int swapInterval, int width, int height) {
    QGLFormat format = QGLFormat::defaultFormat();
    format.setSwapInterval(swapInterval);
    GLContext* result = new GLContext(format);
    result->mainWindow->resize(width, height);
    return result;
};

extern "C" void destroyGLContext(GLContext* ptr) {
    delete ptr;
};

extern "C" void setWindowIcon(GLContext* self, QIcon* icon) {
    self->mainWindow->setWindowIcon(*icon);
};

extern "C" void postGUI(GLContext* self, guiAction* action) {
    self->postGUI(action);
};

extern "C" void setRenderingLooped(GLContext* self, bool looped) {
    if (looped) {
        self->repaintTimer->start();
    } else {
        self->repaintTimer->stop();
    }
};

// sets if auto repeat key events should be processed
extern "C" void setArrowAutoRepeat(GLContext* self, bool status) {
    self->arrowAutoRepeat = status;
};

extern "C" void updateGLContext(GLContext* self) {
    self->update();
};


// sets the window to fullscreen mode.
// In fullscreen mode the mouse cursor is hidden
extern "C" void setFullscreenGLContext(GLContext* ptr, bool fullscreen) {
    // flags are low level but easy: Just think!
    if (fullscreen) {
        ptr->mainWindow->setWindowState(
            ptr->mainWindow->windowState() | Qt::WindowFullScreen);
    } else {
        ptr->mainWindow->setWindowState(
            ptr->mainWindow->windowState() & ~(Qt::WindowFullScreen));
    };
};

extern "C" void setWindowTitle(GLContext* ptr, char* title) {
    ptr->mainWindow->setWindowTitle(QString(title));
}

extern "C" void resizeGLContext(GLContext* ptr, int x, int y) {
    ptr->mainWindow->resize(x, y);
};

extern "C" void showGLContext(GLContext* ptr) {
    ptr->mainWindow->show();
};

extern "C" void hideGLContext(GLContext* ptr) {
    ptr->mainWindow->hide();
};

extern "C" bool directRenderingGLContext(GLContext* ptr) {
    return ptr->format().directRendering();
};

extern "C" int paintEngineTypeGLContext(GLContext* widget) {
    return widget->paintEngine()->type();
};

extern "C" void setDrawingCallbackGLContext(GLContext* ptr, drawingCallbackFunction cb) {
    ptr->drawingCallback = cb;
    // since we have a new drawing callback, we might want to use it ;)
    ptr->update();
};

extern "C" void setKeyCallbackGLContext(GLContext* ptr, keyCallbackFunction cb) {
    ptr->keyCallback = cb;
};
