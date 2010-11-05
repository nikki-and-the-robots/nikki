
#include <QtGui>
#include <QGLWidget>

// * function pointer types
typedef void (drawingCallbackFunction)(QPainter*);

typedef void (keyCallbackFunction) (bool, QKeyEvent*);



class AppWidget : public QGLWidget {

Q_OBJECT

public:

    AppWidget(const QGLFormat& format);

    QTimer* repaintTimer;

    drawingCallbackFunction* drawingCallback;

    keyCallbackFunction* keyCallback;

    bool autoRepeat;

    void paintEvent(QPaintEvent* event);

    void keyPressEvent(QKeyEvent* e);

    void keyReleaseEvent(QKeyEvent* e);

    void setRenderingLooped(bool b);

signals:
    void setRenderingLoopedSignal(bool b);

public slots:
    void setRenderingLoopedSlot(bool b);

};

