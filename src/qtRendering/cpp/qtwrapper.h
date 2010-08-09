
#include <QtGui>
#include <QGLWidget>

// * function pointer types
typedef void (drawingCallbackFunction)(QPainter*);

typedef void (keyCallbackFunction)(bool, int);



class AppWidget : public QGLWidget {

Q_OBJECT

public:

    AppWidget(const QGLFormat& format);

    QTimer* repaintTimer;

    drawingCallbackFunction* drawingCallback;

    keyCallbackFunction* keyCallback;

    void paintEvent(QPaintEvent* event);

    void keyPressEvent(QKeyEvent* e);

    void keyReleaseEvent(QKeyEvent* e);

};

