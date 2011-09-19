
#include <QtGui>
#include <QGLWidget>


// * function pointer types
typedef void (drawingCallbackFunction) (QPainter*);


class GLContext : public QGLWidget {

Q_OBJECT

public:

    GLContext(const QGLFormat& format);

    drawingCallbackFunction* drawingCallback;

    void paintEvent(QPaintEvent* event);

};
