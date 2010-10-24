
#include "qtwrapper.h"

// * utils

char* QStringToCString(QString x) {
    char* arr = (char*) malloc(sizeof(char) * (x.size() + 1));
    for (int i = 0; i < x.size(); i++) {
        arr[i] = x.at(i).toAscii();
    };
    arr[x.size()] = 0;
    return arr;
};

// * debugging

void error(QString msg) {
    qDebug() << "ERROR:" << msg;
    qApp->exit(23);
};

// * AppWidget class

AppWidget::AppWidget(const QGLFormat& format) : QGLWidget(format) {
// AppWidget::AppWidget(const QGLFormat& format) : QWidget() {
    drawingCallback = NULL;
    keyCallback = NULL;

    this->setAutoFillBackground(false);
    this->setCursor(Qt::BlankCursor);

    this->repaintTimer = new QTimer(this);
    QObject::connect(this->repaintTimer, SIGNAL(timeout()), this, SLOT(update()));
};

void AppWidget::paintEvent(QPaintEvent* event) {
    event->accept();

    if (this->drawingCallback != NULL) {
        QPainter painter(this);
        painter.setRenderHints(QPainter::SmoothPixmapTransform, true);
        this->drawingCallback(&painter);
    }
};

void AppWidget::keyPressEvent(QKeyEvent* e) {
    if ((! e->isAutoRepeat()) && (keyCallback != NULL)) {
        this->keyCallback(true, e);
    }
};

void AppWidget::keyReleaseEvent(QKeyEvent* e) {
    if ((! e->isAutoRepeat()) && (keyCallback != NULL)) {
        this->keyCallback(false, e);
    }
};



// ** stuff called by haskell

// * globals

extern "C" const char* cppQVersion() {
    return qVersion();
};


// * QApplication

extern "C" QApplication* newQApplication(char* progName) {
    // Using *argcPtr makes sure it is a real C++-like reference
    // and does not get deleted.
    int* argcPtr = (int*) malloc(sizeof(int));
    *argcPtr = 1;

    char** argv = (char**) malloc(sizeof(char*) * 2);
    argv[0] = progName;
    // this null ending is probably not necessary, 
    // but iirc the c-runtime honours this convention in its arguments to main.
    argv[1] = NULL;

    return new QApplication(*argcPtr, argv, true);
};

extern "C" int execQApplication(QApplication* ptr) {
    return ptr->exec();
}

extern "C" void quitQApplication() {
    qApp->quit();
};

extern "C" char* applicationFilePath() {
    QString path = QCoreApplication::applicationFilePath();
    char* arr = (char*) malloc(sizeof(char) * (path.size() + 1));
    for (int i = 0; i < path.size(); i++) {
        arr[i] = path.at(i).toAscii();
    }
    arr[path.size()] = 0;
    return arr;
};

extern "C" void processEventsQApplication() {
    qDebug() << "flush";
    QApplication::flush();
};

extern "C" void cppSetApplicationName(QApplication* app, char* name) {
    app->setApplicationName(name);
};

extern "C" int desktopWidth(QApplication* app, AppWidget* w) {
    return app->desktop()->availableGeometry(w).width();
};


// * AppWidget

extern "C" AppWidget* newAppWidget(int swapInterval) {
    QGLFormat format = QGLFormat::defaultFormat();
    format.setSwapInterval(swapInterval);
    return new AppWidget(format);
};

extern "C" void setRenderLooped(AppWidget* self, bool looped) {
    if (looped) {
        self->repaintTimer->start();
    } else {
        self->repaintTimer->stop();
    }
};

extern "C" void updateAppWidget(AppWidget* self) {
    self->update();
};


// sets the window to fullscreen mode.
// In fullscreen mode the mouse cursor is hidden
extern "C" void setFullscreenAppWidget(AppWidget* ptr, bool fullscreen) {
    // flags are low level but easy: Just think!
    if (fullscreen) {
        ptr->setWindowState(ptr->windowState() | Qt::WindowFullScreen);
    } else {
        ptr->setWindowState(ptr->windowState() & ~Qt::WindowFullScreen);
    };
};

extern "C" void setWindowTitle(AppWidget* ptr, char* title) {
    ptr->setWindowTitle(QString(title));
}

extern "C" void resizeAppWidget(AppWidget* ptr, int x, int y) {
    ptr->resize(x, y);
};

extern "C" void showAppWidget(AppWidget* ptr) {
    ptr->show();
};

extern "C" void hideAppWidget(AppWidget* ptr) {
    ptr->hide();
};

extern "C" bool directRenderingAppWidget(AppWidget* ptr) {
    return ptr->format().directRendering();
};

extern "C" int paintEngineTypeAppWidget(AppWidget* widget) {
    return widget->paintEngine()->type();
};

extern "C" void setDrawingCallbackAppWidget(AppWidget* ptr, drawingCallbackFunction cb) {
    ptr->drawingCallback = cb;
    // since we have a new drawing callback, we might want to use it ;)
    ptr->update();
};

extern "C" void setKeyCallbackAppWidget(AppWidget* ptr, keyCallbackFunction cb) {
    ptr->keyCallback = cb;
};


// * QPainter

extern "C" void eraseRect(QPainter* painter, int x, int y, int w, int h, int r, int g, int b, int alpha) {
    painter->setBackground(QBrush(QColor(r, g, b, alpha)));
    painter->eraseRect(x, y, w, h);
}

extern "C" void resetMatrix(QPainter* painter) {
    painter->resetMatrix();
};

extern "C" void rotate(QPainter* painter, qreal angle) {
    painter->rotate(angle);
};

extern "C" void cppTranslate(QPainter* painter, qreal x, qreal y) {
    painter->translate(x, y);
};

extern "C" void scale(QPainter* painter, qreal x, qreal y) {
    painter->scale(x, y);
};

extern "C" void cppDrawPixmap(QPainter* painter, int x, int y, QPixmap* pixmap) {
    painter->drawPixmap(x, y, *pixmap);
};

extern "C" void setPenColor(QPainter* painter, int r, int g, int b, int a, int width) {
    QPen pen = QPen(QColor(r, g, b, a));
    pen.setWidth(width);
    painter->setPen(pen);
};

extern "C" void cppDrawRect(QPainter* painter, qreal x, qreal y, qreal w, qreal h) {
    painter->drawRect(x, y, w, h);
};

extern "C" void cppDrawLine(QPainter* painter, qreal x1, qreal y1, qreal x2, qreal y2) {
    QPointF p1 = QPointF(x1, y1);
    QPointF p2 = QPointF(x2, y2);
    painter->drawLine(p1, p2);
};

extern "C" void cppDrawEllipse(QPainter* painter, qreal x, qreal y, qreal w, qreal h) {
    QRectF rect = QRectF(x, y, w, h);
    painter->drawEllipse(rect);
};

extern "C" void cppDrawText(QPainter* painter, qreal x, qreal y, bool highlighted, char* text) {
    painter->setBackgroundMode(Qt::OpaqueMode);

    painter->setBackground(QBrush(QColor(0, 0, 0, 255)));
    if (! highlighted) {
        QPen pen = QPen(QColor(255, 255, 255, 255));
        painter->setPen(pen);
    } else {
        QPen pen = QPen(QColor(255, 0, 0, 255));
        painter->setPen(pen);
    };

    QString qs = QString(text);
    QPointF point = QPointF(x, y);
    painter->drawText(point, qs);
};

extern "C" int widthQPainter(QPainter* painter) {
    return painter->window().width();
};

extern "C" int heightQPainter(QPainter* painter) {
    return painter->window().height();
};



// * QPixmap
extern "C" QPixmap* newQPixmap(char* file) {
    QPixmap* result = new QPixmap(QString(file));
    if (result->isNull())
        qDebug() << "null";
    return result;
};

extern "C" void destroyQPixmap(QPixmap* ptr) {
    return delete ptr;
};

extern "C" int widthQPixmap(QPixmap* ptr) {
    return ptr->width();
};

extern "C" int heightQPixmap(QPixmap* ptr) {
    return ptr->height();
};

// * QTime
extern "C" QTime* newQTime() {
    return new QTime();
};

extern "C" void startQTime(QTime* ptr) {
    ptr->start();
};

extern "C" int restartQTime(QTime* ptr) {
    return ptr->restart();
};

extern "C" int elapsed(QTime* ptr) {
    return ptr->elapsed();
};

// * QKeyEvent
extern "C" int keyQKeyEvent(QKeyEvent* ptr) {
    return ptr->key();
};

extern "C" char* textQKeyEvent(QKeyEvent* ptr) {
    return QStringToCString(ptr->text());
};
