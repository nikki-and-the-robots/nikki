
// * This module contains functions to interact with Qt objects through
// the c calling convention. They are intended to be called by haskell code.
// The Objects are:
//      QApplication
//      QPainter
//      QPixmap
//      QImage
//      QRGB
//      QIcon
//      QTime
//      QString
//      QKeyEvent
//
// The exported functions are far from comprehensive.


#include <QtGui>


// * globals

extern "C" const char* qtVersion() {
    return qVersion();
};

void error(QString msg);

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

    // setting Qt::AA_MacDontSwapCtrlAndMeta doesn't seem to work when the keys are not used as modifiers

    return new QApplication(*argcPtr, argv, true);
};

extern "C" void destroyQApplication(QApplication* ptr) {
    delete ptr;
};

extern "C" int execQApplication(QApplication* ptr) {
    return ptr->exec();
}

extern "C" void quitQApplication() {
    emit qApp->quit();
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

extern "C" void setApplicationName(QApplication* app, char* name) {
    app->setApplicationName(name);
};


// * QPainter

extern "C" void fillRect(QPainter* painter, qreal x, qreal y, qreal w, qreal h, int r, int g, int b, int alpha) {
    painter->setBackground(QBrush(QColor(r, g, b, alpha)));
    painter->eraseRect(x, y, w, h);
};

extern "C" void resetMatrix(QPainter* painter) {
    painter->resetMatrix();
};

extern "C" void rotate(QPainter* painter, qreal angle) {
    painter->rotate(angle);
};

extern "C" void translate(QPainter* painter, qreal x, qreal y) {
    painter->translate(x, y);
};

extern "C" void scale(QPainter* painter, qreal x, qreal y) {
    painter->scale(x, y);
};

extern "C" void drawPixmap(QPainter* painter, qreal x, qreal y, QPixmap* pixmap) {
    painter->drawPixmap(x, y, *pixmap);
};

extern "C" void drawPoint(QPainter* painter, qreal x, qreal y) {
    painter->drawPoint(QPointF(x, y));
};

extern "C" void setPenColor(QPainter* painter, int r, int g, int b, int a, int width) {
    QPen pen = QPen(QColor(r, g, b, a));
    pen.setWidth(width);
    painter->setPen(pen);
};

extern "C" void setFontSize(QPainter* ptr, int size) {
    QFont font = QFont(ptr->font());
    font.setPixelSize(size);
    ptr->setFont(font);
};

extern "C" void drawRect(QPainter* painter, qreal x, qreal y, qreal w, qreal h) {
    painter->drawRect(x, y, w, h);
};

extern "C" void drawLine(QPainter* painter, qreal x1, qreal y1, qreal x2, qreal y2) {
    QPointF p1 = QPointF(x1, y1);
    QPointF p2 = QPointF(x2, y2);
    painter->drawLine(p1, p2);
};

extern "C" void drawEllipse(QPainter* painter, qreal x, qreal y, qreal w, qreal h) {
    QRectF rect = QRectF(x, y, w, h);
    painter->drawEllipse(rect);
};

extern "C" void drawText(QPainter* painter, qreal x, qreal y, bool highlighted, char* text) {
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


// * QTransform

extern "C" void destroyQTransform(QTransform* ptr) {
    delete ptr;
};

extern "C" const QTransform* getMatrix(QPainter* painter) {
    QTransform* matrix = new QTransform(painter->worldTransform());
    return matrix;
};

extern "C" void setMatrix(QPainter* painter, QTransform* matrix) {
    painter->setWorldTransform(*matrix);
};


// * QPixmap
extern "C" QPixmap* newQPixmap(char* file) {
    QPixmap* result = new QPixmap(QString::fromUtf8(file));
    if (result->isNull())
        return NULL;
    return result;
};

extern "C" void destroyQPixmap(QPixmap* ptr) {
    delete ptr;
};

extern "C" QPixmap* copyQPixmap(QPixmap* self) {
    return new QPixmap(*self);
};

extern "C" int widthQPixmap(QPixmap* ptr) {
    return ptr->width();
};

extern "C" int heightQPixmap(QPixmap* ptr) {
    return ptr->height();
};

extern "C" QImage* toImageQPixmap(QPixmap* self) {
    QImage::Format preferred_format = QImage::Format_ARGB32_Premultiplied;
    return new QImage(self->toImage().convertToFormat(preferred_format));
};

extern "C" QPixmap* fromImageQPixmap(QImage* image) {
    return new QPixmap(QPixmap::fromImage(*image));
};


// * QImage
extern "C" void destroyQImage(QImage* self) {
    delete self;
};

extern "C" int widthQImage(QImage* ptr) {
    return ptr->width();
};

extern "C" int heightQImage(QImage* ptr) {
    return ptr->height();
};

extern "C" QRgb pixelQImage(QImage* self, int x, int y) {
    return self->pixel(x, y);
};

extern "C" void setPixelQImage(QImage* self, int x, int y, QRgb rgb) {
    self->setPixel(x, y, rgb);
};


// * QRgb
extern "C" int c_qRed(QRgb x) {return qRed(x);};
extern "C" int c_qGreen(QRgb x) {return qGreen(x);};
extern "C" int c_qBlue(QRgb x) {return qBlue(x);};
extern "C" int c_qAlpha(QRgb x) {return qAlpha(x);};
extern "C" QRgb c_qRgba(int r, int g, int b, int a) {return qRgba(r, g, b, a);};


// * QIcon
extern "C" QIcon* newQIcon() {
    return new QIcon();
};

extern "C" void destroyQIcon(QIcon* ptr) {
    delete ptr;
};

extern "C" void addFileQIcon(QIcon* ptr, char* path) {
    ptr->addFile(QString::fromUtf8(path));
};


// * QTime
extern "C" QTime* newQTime() {
    return new QTime();
};

// * QString

char* QStringToCString(QString x) {
    char* arr = (char*) malloc(sizeof(char) * (x.size() + 1));
    for (int i = 0; i < x.size(); i++) {
        arr[i] = x.at(i).toAscii();
    };
    arr[x.size()] = 0;
    return arr;
};

// * QKeyEvent
extern "C" int keyQKeyEvent(QKeyEvent* ptr) {
    return ptr->key();
};

extern "C" char* textQKeyEvent(QKeyEvent* ptr) {
    return QStringToCString(ptr->text());
};
