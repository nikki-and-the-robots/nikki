
#include "utils.h"
#include "MainWindow.h"

MainWindow::MainWindow(int swapInterval, int width, int height) {
    arrowAutoRepeat = true;
    keyCallback = NULL;
    repaintTimer = new QTimer(this);

    QVBoxLayout* layout = new QVBoxLayout();
    layout->setContentsMargins(QMargins());
    this->setLayout(layout);
    resize(width, height);

    QObject::connect(this, SIGNAL(postGUISignal(guiAction*)),
                     this, SLOT(postGUISlot(guiAction*)));

    // child widget
    QGLFormat format = QGLFormat::defaultFormat();
    format.setSwapInterval(swapInterval);
    glContext = new GLContext(format);
    if (glContext->isValid()) {
        fallbackContext = 0;
        generalContext = glContext;

        glContext->drawingCallback = emptyDrawingCallback;
    } else {
        glContext = 0;
        fallbackContext = new FallbackContext();
        generalContext = fallbackContext;

        fallbackContext->drawingCallback = emptyDrawingCallback;
    };

    generalContext->setAutoFillBackground(false);
    generalContext->setCursor(Qt::BlankCursor);
    generalContext->setFocusPolicy(Qt::StrongFocus);

    QObject::connect(this->repaintTimer, SIGNAL(timeout()),
                     generalContext, SLOT(update()));
    layout->addWidget(generalContext);
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
    delete ptr->generalContext;
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
    self->generalContext->update();
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
    if (ptr->glContext != 0)
        return ptr->glContext->format().directRendering();
    else
        return false;
};

extern "C" int paintEngineTypeMainWindow(MainWindow* self) {
    return self->generalContext->paintEngine()->type();
};

extern "C" void setDrawingCallbackMainWindow(MainWindow* ptr, drawingCallbackFunction* cb) {
    if (ptr->glContext != 0)
        ptr->glContext->drawingCallback = cb;
    else
        ptr->fallbackContext->drawingCallback = cb;
    // since we have a new drawing callback, we might want to use it ;)
    ptr->generalContext->update();
};

// the postGUI signal-slot-pair is necessary to execute a command in the
// GUI thread.
void MainWindow::postGUI(guiAction* action) {
    emit postGUISignal(action);
};

// cpp has to call this on every guiAction function pointer
// that gets passed to postGUI to let haskell release the retained memory.
// (After performing the given action, of course.)
extern "C" void freePostGUIFunPtr(guiAction* action);

void MainWindow::postGUISlot(guiAction* action) {
    action();
    freePostGUIFunPtr(action);
};
