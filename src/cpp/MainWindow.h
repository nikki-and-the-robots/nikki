
// This implements a main window as a container for
// a GLContext. It's pretty stupid itself and
// mainly operated by the GLContext.
// It's implemented as a workaround to
// http://bugreports.qt.nokia.com/browse/QTBUG-17340

#include <QtGui>
#include <QtOpenGL>

#include "GLContext.h"

                                          // nullptr for window close event
typedef void (keyCallbackFunction) (int eventCode, QKeyEvent*);
// int being an event code:
//  0 - press key event (with the QKeyEvent)
//  1 - release key event (with the QKeyEvent)
//  2 - focus out of window event
//  3 - window close event (from the window manager)


// type for arbitrary actions to be performed in the GUI thread
typedef void (guiAction) ();


class MainWindow : public QWidget {

Q_OBJECT

public:

    MainWindow(int swapInterval, int width, int height);
    GLContext* child;

    // embeds a child widget
    void setChild(GLContext* child);

    // key callbacks
    keyCallbackFunction* keyCallback;
    bool arrowAutoRepeat;
    void keyPressEvent(QKeyEvent* e);
    void keyReleaseEvent(QKeyEvent* e);
    void focusOutEvent(QFocusEvent* e);
    void closeEvent(QCloseEvent* e);

    // timer for repainting
    QTimer* repaintTimer;

    // postGUI stuff
    void postGUI(guiAction* action);
signals:
    void postGUISignal(guiAction* action);
public slots:
    void postGUISlot(guiAction* action);

};
