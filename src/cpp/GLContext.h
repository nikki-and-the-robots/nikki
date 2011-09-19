
#include <QtGui>
#include <QGLWidget>


// * function pointer types
typedef void (drawingCallbackFunction) (QPainter*);

// type for arbitrary actions to be performed in the GUI thread
typedef void (guiAction) ();


class GLContext : public QGLWidget {

Q_OBJECT

public:

    QWidget* mainWindow;

    GLContext(const QGLFormat& format);

    drawingCallbackFunction* drawingCallback;

    void paintEvent(QPaintEvent* event);

    void postGUI(guiAction* action);

signals:
    void postGUISignal(guiAction* action);

public slots:
    void postGUISlot(guiAction* action);

};

