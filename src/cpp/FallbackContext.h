

#include <QWidget>

#include "utils.h"


class FallbackContext : public QWidget {

Q_OBJECT

public:

    FallbackContext();

    drawingCallbackFunction* drawingCallback;

    void paintEvent(QPaintEvent* event);

};
