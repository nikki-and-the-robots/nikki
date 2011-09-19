
#include <QtGui>


void error(QString msg);

bool isArrowKey(QKeyEvent* e);

// * function pointer types
typedef void (drawingCallbackFunction) (QPainter*);
