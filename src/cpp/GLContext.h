
#include <QtGui>
#include <QGLWidget>

#include "utils.h"

class GLContext : public QGLWidget {

Q_OBJECT

private:

    drawingCallbackFunction* drawingCallback;
    drawingCallbackFunction* oldDrawingCallback;

public:

    GLContext(const QGLFormat& format);

    void setDrawingCallback(drawingCallbackFunction* dcb);

    void paintEvent(QPaintEvent* event);

};
