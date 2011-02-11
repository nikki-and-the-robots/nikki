
// This implements a main window as a container for
// a GLContext. It's pretty stupid itself and
// mainly operated by the GLContext.
// It's implemented as a workaround to
// http://bugreports.qt.nokia.com/browse/QTBUG-17340

#include <QtGui>
#include <QtOpenGL>

#include "GLContext.h"


class MainWindow : public QWidget {

Q_OBJECT

private:

    GLContext* child;

public:

    MainWindow();

    // embeds a child widget
    void setChild(GLContext* child);

    void keyPressEvent(QKeyEvent* e);

    void keyReleaseEvent(QKeyEvent* e);

    void closeEvent(QCloseEvent* e);

};
