
#include <QtGui>
#include <QGLWidget>


// * function pointer types
typedef void (drawingCallbackFunction) (QPainter*);

                                          // nullptr for window close event
typedef void (keyCallbackFunction) (bool, QKeyEvent*);

// type for arbitrary actions to be performed in the GUI thread
typedef void (guiAction) ();


class GLContext : public QGLWidget {

Q_OBJECT

public:

    QWidget* mainWindow;

    GLContext(const QGLFormat& format);

    QTimer* repaintTimer;

    drawingCallbackFunction* drawingCallback;

    keyCallbackFunction* keyCallback;

    bool arrowAutoRepeat;

    void paintEvent(QPaintEvent* event);

    void keyPressEvent(QKeyEvent* e);

    void keyReleaseEvent(QKeyEvent* e);

    void closeEvent(QCloseEvent* e);

    void postGUI(guiAction* action);

signals:
    void postGUISignal(guiAction* action);

public slots:
    void postGUISlot(guiAction* action);

};

