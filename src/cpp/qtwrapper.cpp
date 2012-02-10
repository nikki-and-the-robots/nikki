
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

extern "C" bool qtOpenUrl(char* url) {
    return QDesktopServices::openUrl(QUrl(QString(url)));
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

extern "C" void processEventsQApplication() {
    QCoreApplication::processEvents();
};

extern "C" void setApplicationName(QApplication* app, char* name) {
    app->setApplicationName(name);
};


// * QPainter

// for rendering into QPixmaps
extern "C" QPainter* newQPainter(QPixmap* pixmap) {
    QPainter* r = new QPainter(pixmap);
    r->setRenderHints(QPainter::SmoothPixmapTransform, false);
    r->setRenderHints(QPainter::Antialiasing, false);
    r->setRenderHints(QPainter::TextAntialiasing, false);
    return r;
};

extern "C" void destroyQPainter(QPainter* ptr) {
    delete ptr;
};

extern "C" void fillRect(QPainter* painter, qreal x, qreal y, qreal w, qreal h, int r, int g, int b, int alpha) {
    painter->setBackground(QBrush(QColor(r, g, b, alpha)));
    painter->eraseRect(x, y, w, h);
};

extern "C" void resetMatrix(QPainter* painter) {
    painter->resetMatrix();
};

extern "C" void setCompositionModeDefault(QPainter* ptr) {
    ptr->setCompositionMode(QPainter::CompositionMode_SourceOver);
};

extern "C" void setCompositionModeClear(QPainter* ptr) {
    ptr->setCompositionMode(QPainter::CompositionMode_Clear);
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

// without force_align_arg_pointer this function can cause a segfault on windows when the painter paints on an
// in memory QPixmap
extern "C" void __attribute__((force_align_arg_pointer)) drawPixmap(QPainter* painter, qreal x, qreal y, QPixmap* pixmap) {
    painter->drawPixmap(x, y, *pixmap);
};

extern "C" void drawPoint(QPainter* painter, int x, int y) {
    painter->drawPoint(x, y);
};

extern "C" void setPenColor(QPainter* painter, int r, int g, int b, int a, int width) {
    QPen pen = QPen(QColor(r, g, b, a));
    pen.setWidth(width);
    painter->setPen(pen);
};

extern "C" void setClipRect(QPainter* self, qreal x, qreal y, qreal w, qreal h) {
    self->setClipRect(QRectF(x, y, w, h));
};

extern "C" void setClipping(QPainter* self, bool clipping) {
    self->setClipping(clipping);
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


// * drawing pixmaps with drawPixmapFragments

// drawPixmapFragments can be used when rendering the same pixmap multiple times.
// There is a global array of PixmapFragments kept.
// Rendering of one pixmap n times works like this:
// 1. writePixmapFragmentArray n times with array indices [0 .. (n - 1)]
// 2. drawPixmapFragments(painter, n, pixmap)
// 
// None of this is re-entrant

const int maxNumberOfPixmapFragments = 10;

QPainter::PixmapFragment* initializePixmapFragmentArray() {
    QPainter::PixmapFragment* array = new QPainter::PixmapFragment[maxNumberOfPixmapFragments];
    for (int i = 0; i < maxNumberOfPixmapFragments; i++) {
        QPainter::PixmapFragment* f = &array[i];
        // sourceLeft and sourceTop are not used in nikki
        f->sourceLeft = 0;
        f->sourceTop = 0;
        f->scaleX = 1;
        f->scaleY = 1;
        f->opacity = 1;
    };
    return array;
};

QPainter::PixmapFragment* pixmapFragmentArray = initializePixmapFragmentArray();

extern "C" void writePixmapFragmentArray
    (int index, qreal x, qreal y, qreal angle, QPixmap* pixmap) {
    QPainter::PixmapFragment* f = &pixmapFragmentArray[index];
    f->x = x;
    f->y = y;
    f->width = pixmap->rect().width();
    f->height = pixmap->rect().height();
    f->rotation = angle;
};

extern "C" void drawPixmapFragments (QPainter* ptr, int length, QPixmap* pixmap) {
    qDebug() <<
        "Warning: drawPixmapFragments (c++-land): only 10 fragments can be rendered.";
    ptr->drawPixmapFragments(pixmapFragmentArray, length, *pixmap);
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

extern "C" QPixmap* newQPixmapEmpty(int width, int height) {
    QPixmap* r = new QPixmap(width, height);
    r->fill(QColor(0, 0, 0, 0));
    return r;
};

extern "C" void destroyQPixmap(QPixmap* ptr) {
    delete ptr;
};

extern "C" void saveQPixmap(QPixmap* ptr, char* file, int quality) {
    ptr->save(QString(file), 0, quality);
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

extern "C" QImage* toImageQPixmap(QPixmap* self, bool useIndexed8) {
    if (useIndexed8) {
        QImage::Format preferred_format = QImage::Format_Indexed8;
        return new QImage(self->toImage().convertToFormat(preferred_format));
    } else {
        return new QImage(self->toImage());
    };
};

extern "C" QPixmap* fromImageQPixmap(QImage* image) {
    return new QPixmap(QPixmap::fromImage(*image));
};


// * QImage
extern "C" void destroyQImage(QImage* self) {
    delete self;
};

extern "C" void saveQImage(QImage* self, char* file) {
    self->save(QString(file), 0, 0);
};

extern "C" int widthQImage(QImage* ptr) {
    return ptr->width();
};

extern "C" int heightQImage(QImage* ptr) {
    return ptr->height();
};

extern "C" int colorCountQImage(QImage* ptr) {
    return ptr->colorCount();
};

extern "C" QRgb colorQImage(QImage* ptr, int index) {
    return ptr->color(index);
};

extern "C" void setColorQImage(QImage* ptr, int index, QRgb color) {
    ptr->setColor(index, color);
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

// | creates a new QByteArray
QByteArray* QStringToCString(QString x) {
    return new QByteArray(x.toUtf8());
};

// * QKeyEvent
extern "C" int keyQKeyEvent(QKeyEvent* ptr) {
    return ptr->key();
};

extern "C" QByteArray* textQKeyEvent(QKeyEvent* ptr) {
    return QStringToCString(ptr->text());
};

extern "C" int modifiersQKeyEvent(QKeyEvent* ptr) {
    return ptr->modifiers();
};

// * QByteArray
extern "C" void destroyQByteArray(QByteArray* ptr) {
    delete ptr;
};

extern "C" char* dataQByteArray(QByteArray* ptr) {
    return ptr->data();
};

// * QClipboard
extern "C" QByteArray* textQClipboard() {
    return QStringToCString(QApplication::clipboard()->text());
};
