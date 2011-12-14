
#include <QtGui>
#include <QGLWidget>

#include "utils.h"

class GLContext : public QGLWidget {

Q_OBJECT

public:

    GLContext(const QGLFormat& format);

    drawingCallbackFunction* drawingCallback;

    void paintEvent(QPaintEvent* event);

};
