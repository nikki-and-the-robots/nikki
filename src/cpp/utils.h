
#include <QtGui>


void error(QString msg);

bool isArrowKey(QKeyEvent* e);

// * function pointer types
typedef void (drawingCallbackFunction) (QPainter*);

void emptyDrawingCallback(QPainter* ptr);

// From haskell. Has to be called to free the drawing callback.
extern "C" void freeDrawingCallback(drawingCallbackFunction* action);
