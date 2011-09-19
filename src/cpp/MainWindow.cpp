
#include "utils.h"
#include "MainWindow.h"

void emptyDrawingCallback(QPainter* ptr) {
};

MainWindow::MainWindow(int swapInterval, int width, int height) {
    arrowAutoRepeat = true;
    keyCallback = NULL;
    repaintTimer = new QTimer(this);

    QVBoxLayout* layout = new QVBoxLayout();
    layout->setContentsMargins(QMargins());
    this->setLayout(layout);

    // child widget
    QGLFormat format = QGLFormat::defaultFormat();
    format.setSwapInterval(swapInterval);

    child = new GLContext(format);
    if (!(child->isValid())) {
        error("GL context not valid");
    };
    child->drawingCallback = emptyDrawingCallback;
    child->setAutoFillBackground(false);
    child->setCursor(Qt::BlankCursor);
    child->setFocusPolicy(Qt::StrongFocus);

    QObject::connect(this->repaintTimer, SIGNAL(timeout()),
                     child, SLOT(update()));
    QObject::connect(this, SIGNAL(postGUISignal(guiAction*)),
                     this, SLOT(postGUISlot(guiAction*)));
    layout->addWidget(child);

    resize(width, height);
};



void MainWindow::keyPressEvent(QKeyEvent* e) {
    if (((this->arrowAutoRepeat && isArrowKey(e)) || (! e->isAutoRepeat())) && (keyCallback != NULL)) {
        e->accept();
        this->keyCallback(0, e);
    }
};

void MainWindow::keyReleaseEvent(QKeyEvent* e) {
    if ((! e->isAutoRepeat()) && (keyCallback != NULL)) {
        e->accept();
        this->keyCallback(1, e);
    }
};

void MainWindow::focusOutEvent(QFocusEvent* e) {
    if (keyCallback != NULL) {
        this->keyCallback(2, NULL);
    }
};

void MainWindow::closeEvent(QCloseEvent* e) {
    e->ignore();
    if (keyCallback != NULL) {
        this->keyCallback(3, NULL);
    }
};


// * external C

// sets if auto repeat key events should be processed
extern "C" void setArrowAutoRepeat(MainWindow* self, bool status) {
    self->arrowAutoRepeat = status;
};

extern "C" void setKeyCallbackMainWindow(MainWindow* ptr, keyCallbackFunction cb) {
    ptr->keyCallback = cb;
};


// * external C

extern "C" MainWindow* newMainWindow(int swapInterval, int width, int height) {
    return new MainWindow(swapInterval, width, height);
};

extern "C" void destroyMainWindow(MainWindow* ptr) {
    delete ptr->child;
    delete ptr;
};

extern "C" void setWindowIcon(MainWindow* self, QIcon* icon) {
    self->setWindowIcon(*icon);
};

extern "C" void postGUI(MainWindow* self, guiAction* action) {
    self->postGUI(action);
};

extern "C" void setRenderingLooped(MainWindow* self, bool looped) {
    if (looped) {
        self->repaintTimer->start();
    } else {
        self->repaintTimer->stop();
    }
};

extern "C" void updateMainWindow(MainWindow* self) {
    self->child->update();
};


// sets the window to fullscreen mode.
// In fullscreen mode the mouse cursor is hidden
extern "C" void setFullscreenMainWindow(MainWindow* ptr, bool fullscreen) {
    // flags are low level but easy: Just think!
    if (fullscreen) {
        ptr->setWindowState(
            ptr->windowState() | Qt::WindowFullScreen);
    } else {
        ptr->setWindowState(
            ptr->windowState() & ~(Qt::WindowFullScreen));
    };
};

extern "C" void setWindowTitle(MainWindow* ptr, char* title) {
    ptr->setWindowTitle(QString(title));
}

extern "C" void resizeMainWindow(MainWindow* ptr, int x, int y) {
    ptr->resize(x, y);
};

extern "C" void showMainWindow(MainWindow* ptr) {
    ptr->show();
    ptr->raise();
};

extern "C" void hideMainWindow(MainWindow* ptr) {
    ptr->hide();
};

extern "C" bool directRenderingMainWindow(MainWindow* ptr) {
    return ptr->child->format().directRendering();
};

extern "C" int paintEngineTypeMainWindow(MainWindow* self) {
    return self->child->paintEngine()->type();
};

extern "C" void setDrawingCallbackMainWindow(MainWindow* ptr, drawingCallbackFunction cb) {
    ptr->child->drawingCallback = cb;
    // since we have a new drawing callback, we might want to use it ;)
    ptr->child->update();
};

// the postGUI signal-slot-pair is necessary to execute a command in the
// GUI thread.
void MainWindow::postGUI(guiAction* action) {
    emit postGUISignal(action);
};

void MainWindow::postGUISlot(guiAction* action) {
    action();
};
