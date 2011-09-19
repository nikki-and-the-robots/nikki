

#include <QWidget>

#include "utils.h"


class FallbackContext : public QWidget {

Q_OBJECT

public:
    drawingCallbackFunction* drawingCallback;

    void paintEvent(QPaintEvent* event);

};
