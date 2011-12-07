

#include <QWidget>

#include "utils.h"


class FallbackContext : public QWidget {

Q_OBJECT

private:

    drawingCallbackFunction* drawingCallback;
    drawingCallbackFunction* oldDrawingCallback;

public:

    FallbackContext();

    void setDrawingCallback(drawingCallbackFunction* dcb);

    void paintEvent(QPaintEvent* event);

};
